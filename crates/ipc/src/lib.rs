//! IPC protocol and transport primitives for daemon/client communication.

use std::io::{BufRead, BufReader, Write};

use holo_base::{holo_message_error, FilePath, Result, SharedString, SourceDiagnostic};
use interprocess::local_socket::traits::{Listener as _, Stream as _};
use interprocess::local_socket::{GenericNamespaced, Listener, ListenerOptions, Stream, ToNsName};
use serde::{Deserialize, Serialize};

/// Client request variants sent to the daemon.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum Request {
    /// Requests a one-shot build cycle.
    Build,
    /// Requests current daemon status information.
    GetStatus,
    /// Requests current issue snapshot for deck/client views.
    GetIssues,
    /// Subscribes to daemon-initiated issue update events.
    SubscribeIssues,
    /// Requests daemon shutdown.
    Shutdown,
}

/// Daemon response variants sent to clients.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum Response {
    /// Generic success response.
    Ok,
    /// Daemon status report output.
    StatusReport(SharedString),
    /// Current issue snapshot.
    IssuesSnapshot(Vec<ProjectIssue>),
    /// Error response with message.
    Error(SharedString),
}

/// Daemon-initiated message variants broadcast or pushed to clients.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum DaemonEvent {
    /// Emitted when a daemon cycle report is produced.
    CycleReport(SharedString),
    /// Emitted when issue set changes.
    IssuesUpdated(Vec<ProjectIssue>),
    /// Emitted when the dependency graph representation changes.
    DependencyGraph(SharedString),
    /// Emitted when daemon lifecycle changes.
    Lifecycle(SharedString),
    /// Emitted when the latest cycle timings are updated.
    PerformanceTimings(Vec<SharedString>),
}

/// Project issue payload exchanged over IPC.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectIssue {
    /// Short issue title.
    pub title: SharedString,
    /// Source file path for the issue.
    pub file: FilePath,
    /// 1-based line number.
    pub line: usize,
    /// High-level issue category.
    pub kind: ProjectIssueKind,
    /// Severity category.
    pub severity: ProjectIssueSeverity,
    /// Compact summary text.
    pub summary: SharedString,
    /// Detailed human-readable message.
    pub detail: SharedString,
    /// Structured source diagnostics for compilation issues.
    pub source_diagnostics: Vec<SourceDiagnostic>,
}

/// High-level issue category.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProjectIssueKind {
    Compilation,
    Test,
}

/// Issue severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProjectIssueSeverity {
    Error,
    Warning,
}

/// Bidirectional wire message envelope for JSON transport.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "payload")]
pub enum WireMessage {
    /// Request issued by a client.
    Request { request_id: u64, request: Request },
    /// Response paired with a prior request.
    Response { request_id: u64, response: Response },
    /// Daemon-initiated event without request correlation.
    Event { event: DaemonEvent },
}

/// IPC server that accepts local socket connections.
pub struct IpcServer {
    listener: Listener,
}

impl IpcServer {
    /// Binds a new IPC server to the given endpoint name.
    pub fn bind(endpoint_name: &str) -> Result<Self> {
        let name = endpoint_name
            .to_ns_name::<GenericNamespaced>()
            .map_err(|error| {
                holo_message_error!("invalid IPC endpoint name").with_std_source(error)
            })?;
        let listener = ListenerOptions::new()
            .name(name)
            .create_sync()
            .map_err(|error| {
                holo_message_error!("failed to bind IPC server").with_std_source(error)
            })?;
        Ok(Self { listener })
    }

    /// Accepts one incoming connection.
    pub fn accept(&self) -> Result<IpcConnection> {
        let stream = self.listener.accept().map_err(|error| {
            holo_message_error!("failed to accept IPC client").with_std_source(error)
        })?;
        Ok(IpcConnection::new(stream))
    }
}

/// Bidirectional IPC connection.
pub struct IpcConnection {
    reader: BufReader<Stream>,
}

impl IpcConnection {
    fn new(stream: Stream) -> Self {
        Self {
            reader: BufReader::new(stream),
        }
    }

    /// Connects to a daemon endpoint as a client.
    pub fn connect(endpoint_name: &str) -> Result<Self> {
        let name = endpoint_name
            .to_ns_name::<GenericNamespaced>()
            .map_err(|error| {
                holo_message_error!("invalid IPC endpoint name").with_std_source(error)
            })?;
        let stream = Stream::connect(name).map_err(|error| {
            holo_message_error!("failed to connect to IPC endpoint").with_std_source(error)
        })?;
        Ok(Self::new(stream))
    }

    /// Sends one wire message encoded as JSON line.
    pub fn send(&mut self, message: &WireMessage) -> Result<()> {
        let mut encoded = serde_json::to_vec(message).map_err(|error| {
            holo_message_error!("failed to encode IPC message").with_std_source(error)
        })?;
        encoded.push(b'\n');
        self.reader.get_mut().write_all(&encoded).map_err(|error| {
            holo_message_error!("failed to write IPC message").with_std_source(error)
        })?;
        self.reader.get_mut().flush().map_err(|error| {
            holo_message_error!("failed to flush IPC message").with_std_source(error)
        })
    }

    /// Receives one wire message decoded from JSON line.
    pub fn receive(&mut self) -> Result<Option<WireMessage>> {
        let mut line = std::string::String::new();
        let bytes_read = self.reader.read_line(&mut line).map_err(|error| {
            holo_message_error!("failed to read IPC message").with_std_source(error)
        })?;
        if bytes_read == 0 {
            return Ok(None);
        }

        let message =
            serde_json::from_str::<WireMessage>(line.trim_end_matches('\n')).map_err(|error| {
                holo_message_error!("failed to decode IPC message").with_std_source(error)
            })?;
        Ok(Some(message))
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DaemonEvent, ProjectIssue, ProjectIssueKind, ProjectIssueSeverity, Request, Response,
        WireMessage,
    };

    #[test]
    fn serializes_request_response_and_event_messages() {
        let request_message = WireMessage::Request {
            request_id: 7,
            request: Request::Build,
        };
        let request_json =
            serde_json::to_string(&request_message).expect("request should serialize");
        let request_round_trip: WireMessage =
            serde_json::from_str(&request_json).expect("request should deserialize");
        assert_eq!(request_round_trip, request_message);

        let response_message = WireMessage::Response {
            request_id: 7,
            response: Response::StatusReport("ok".into()),
        };
        let response_json =
            serde_json::to_string(&response_message).expect("response should serialize");
        let response_round_trip: WireMessage =
            serde_json::from_str(&response_json).expect("response should deserialize");
        assert_eq!(response_round_trip, response_message);

        let event_message = WireMessage::Event {
            event: DaemonEvent::CycleReport("report".into()),
        };
        let event_json = serde_json::to_string(&event_message).expect("event should serialize");
        let event_round_trip: WireMessage =
            serde_json::from_str(&event_json).expect("event should deserialize");
        assert_eq!(event_round_trip, event_message);

        let issue = ProjectIssue {
            title: "assertion failed".into(),
            file: "tests/auth.holo".into(),
            line: 10,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Error,
            summary: "login test failed".into(),
            detail: "assert(false) evaluated to false".into(),
            source_diagnostics: Vec::new(),
        };
        let issue_response = WireMessage::Response {
            request_id: 8,
            response: Response::IssuesSnapshot(vec![issue.clone()]),
        };
        let issue_response_json =
            serde_json::to_string(&issue_response).expect("issue response should serialize");
        let issue_response_round_trip: WireMessage =
            serde_json::from_str(&issue_response_json).expect("issue response should deserialize");
        assert_eq!(issue_response_round_trip, issue_response);

        let issue_event = WireMessage::Event {
            event: DaemonEvent::IssuesUpdated(vec![issue]),
        };
        let issue_event_json =
            serde_json::to_string(&issue_event).expect("issue event should serialize");
        let issue_event_round_trip: WireMessage =
            serde_json::from_str(&issue_event_json).expect("issue event should deserialize");
        assert_eq!(issue_event_round_trip, issue_event);

        let graph_event = WireMessage::Event {
            event: DaemonEvent::DependencyGraph("a.holo -> parser".into()),
        };
        let graph_event_json =
            serde_json::to_string(&graph_event).expect("graph event should serialize");
        let graph_event_round_trip: WireMessage =
            serde_json::from_str(&graph_event_json).expect("graph event should deserialize");
        assert_eq!(graph_event_round_trip, graph_event);

        let performance_event = WireMessage::Event {
            event: DaemonEvent::PerformanceTimings(vec![
                "18.50ms parse `a.holo`".into(),
                "7.20ms typecheck test `smoke`".into(),
            ]),
        };
        let performance_event_json =
            serde_json::to_string(&performance_event).expect("performance event should serialize");
        let performance_event_round_trip: WireMessage =
            serde_json::from_str(&performance_event_json)
                .expect("performance event should deserialize");
        assert_eq!(performance_event_round_trip, performance_event);
    }
}
