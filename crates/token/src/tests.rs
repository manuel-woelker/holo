use crate::{Token, TokenKind};
use holo_base::Span;

#[test]
fn creates_token_with_kind_and_span() {
    let token = Token::new(TokenKind::Identifier, Span::new(0, 4));
    assert_eq!(token.kind, TokenKind::Identifier);
    assert_eq!(token.span, Span::new(0, 4));
}
