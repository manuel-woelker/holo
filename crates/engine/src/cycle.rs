use super::Engine;

/// Represents the state and logic of a single compilation cycle.
pub struct Cycle<'a> {
    #[allow(dead_code)]
    engine: &'a mut Engine,
}

impl<'a> Cycle<'a> {
    /// Creates a new cycle for the given engine.
    pub fn new(engine: &'a mut Engine) -> Self {
        Self { engine }
    }

    /// Runs the cycle logic.
    pub fn run(&mut self) {
        // Stages 1-5 will be implemented here.
    }
}
