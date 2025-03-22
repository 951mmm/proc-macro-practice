use std::collections::VecDeque;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use quote::TokenStreamExt;
use syn::{braced, parse::Parse, parse_macro_input, Ident, LitInt, RangeLimits, Token};

struct Seq {
    ident: Ident,
    st: LitInt,
    ed: LitInt,
    range_limits: RangeLimits,
    content: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![in]>()?;
        let st = input.parse::<LitInt>()?;
        let range_limits = input.parse::<RangeLimits>()?;
        let ed = input.parse::<LitInt>()?;
        braced!(content in input);
        let content = content.parse::<proc_macro2::TokenStream>()?;
        Ok(Seq {
            ident,
            st,
            ed,
            range_limits,
            content,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    let Seq {
        content,
        st,
        ed,
        range_limits,
        ident,
    } = seq;
    let st = st.base10_parse::<i32>().unwrap();
    let mut ed = ed.base10_parse::<i32>().unwrap();
    if matches!(range_limits, RangeLimits::Closed(_)) {
        ed += 1;
    }
    let mut parser = Parser {
        st,
        ed,
        ident,
        flag: false,
        depth: 0,
    };

    let result = parser.parse(content);

    result.into()
}

enum ParseState {
    Start,
    Pound(PoundState),
    End,
}

enum PoundState {
    Start,
    Section,
    DeriveEnd,
    SectionEnd,
}
/// dfa state
/// Start -> Ident -> Ident~ -> Ident~N -> Ident~N~ -> Ident~N~Ident
///   |        |                  |                       |
///   +---------------<-----------<-----------<-----------+
///   |
///  End
enum State {
    Start,
    Ident,
    IdentTilde,
    IdentTildeN,
    IdentTildeNTilde,
    IdentTildeNTildeIdent,
    End,
}

struct Parser {
    st: i32,
    ed: i32,
    ident: Ident,
    flag: bool,
    depth: u32,
}

impl Parser {
    fn parse(&mut self, token_stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let mut iter = token_stream.into_iter();
        let mut state = ParseState::Start;
        let mut result = proc_macro2::TokenStream::new();
        let mut look_ahead_stack = vec![];
        let mut derive_macro_stack: VecDeque<TokenTree> = VecDeque::new();

        loop {
            match state {
                ParseState::Start => {
                    let cur = iter.next();
                    if let Some(cur) = cur {
                        match cur {
                            TokenTree::Group(group) => {
                                result.append(self.parse_group(group));
                            }
                            TokenTree::Punct(punct) => match punct.as_char() {
                                '#' => {
                                    look_ahead_stack.push(punct);
                                    state = ParseState::Pound(PoundState::Start);
                                }
                                _ => {
                                    result.append(punct);
                                }
                            },
                            _ => {
                                result.append(cur);
                            }
                        }
                    } else {
                        state = ParseState::End;
                    }
                }
                ParseState::Pound(PoundState::Start) => {
                    // look ahead
                    debug_assert_eq!(look_ahead_stack.len(), 1);
                    let cur = iter.next();
                    if let Some(cur) = cur {
                        match cur {
                            TokenTree::Group(group) => {
                                match group.delimiter() {
                                    Delimiter::Bracket => {
                                        derive_macro_stack
                                            .push_back(look_ahead_stack.pop().unwrap().into());
                                        derive_macro_stack.push_back(group.into());
                                        state = ParseState::Pound(PoundState::DeriveEnd);
                                    }
                                    Delimiter::Parenthesis => {
                                        self.flag = true;
                                        look_ahead_stack.pop();
                                        result.extend(self.parse_repeat_group(group));
                                        state = ParseState::Pound(PoundState::Section);
                                    }
                                    _ => {
                                        // error
                                        state = ParseState::Start;
                                    }
                                };
                            }

                            _ => {
                                // error
                                state = ParseState::Start;
                            }
                        }
                    } else {
                        state = ParseState::End;
                    }
                }
                ParseState::Pound(PoundState::DeriveEnd) => {
                    debug_assert_eq!(derive_macro_stack.len(), 2);
                    while let Some(derive_token) = derive_macro_stack.pop_front() {
                        result.append(derive_token);
                    }
                    state = ParseState::Start;
                }
                ParseState::Pound(PoundState::Section) => {
                    if let Some(cur) = iter.next() {
                        if let TokenTree::Punct(punct) = cur {
                            if matches!(punct.as_char(), '*') {
                                state = ParseState::Pound(PoundState::SectionEnd);
                                continue;
                            }
                        }
                        // error
                        state = ParseState::Start;
                    } else {
                        state = ParseState::End;
                    }
                }
                ParseState::Pound(PoundState::SectionEnd) => {
                    state = ParseState::Start;
                }
                ParseState::End => {
                    break;
                }
            }
        }

        if !self.flag && self.depth == 0 {
            result = self.parse_repeat_section(result);
        }
        result
    }
    fn parse_group(&mut self, group: Group) -> Group {
        self.depth += 1;
        let new_group_stream = self.parse(group.stream());
        self.depth -= 1;
        let mut new_group = Group::new(group.delimiter(), new_group_stream);
        new_group.set_span(group.span());
        new_group
    }

    fn parse_repeat_group(&self, group: Group) -> proc_macro2::TokenStream {
        self.parse_repeat_section(group.stream())
    }

    fn parse_repeat_section(&self, section: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let mut repeat_section_expanded = proc_macro2::TokenStream::new();
        for n in self.st..self.ed {
            repeat_section_expanded.extend(self.replace_n(section.clone(), n));
        }
        repeat_section_expanded
    }

    fn replace_n(
        &self,
        token_stream: proc_macro2::TokenStream,
        n: i32,
    ) -> proc_macro2::TokenStream {
        let mut result = proc_macro2::TokenStream::new();
        let mut iter = token_stream.into_iter();
        let mut state = State::Start;
        let mut ident_stack = VecDeque::new();
        let mut look_ahead_stack = vec![];

        loop {
            match state {
                State::Start => {
                    let cur = if !look_ahead_stack.is_empty() {
                        debug_assert!(look_ahead_stack.len() <= 1);
                        look_ahead_stack.pop()
                    } else {
                        iter.next()
                    };

                    if let Some(cur) = cur {
                        match cur {
                            TokenTree::Group(group) => {
                                self.flush_ident_stack(&mut result, &mut ident_stack, n);
                                let new_group_stream = self.replace_n(group.stream(), n);
                                let mut new_group =
                                    proc_macro2::Group::new(group.delimiter(), new_group_stream);
                                new_group.set_span(group.span());
                                result.append(new_group);
                            }
                            TokenTree::Punct(punct) => {
                                if matches!(punct.as_char(), '~') {
                                    debug_assert!(ident_stack.len() > 0);
                                    state = State::Ident;
                                } else {
                                    self.flush_ident_stack(&mut result, &mut ident_stack, n);
                                    result.append(punct);
                                }
                            }
                            TokenTree::Ident(ident_inner) => {
                                ident_stack.push_back(ident_inner);
                            }
                            _ => {
                                self.flush_ident_stack(&mut result, &mut ident_stack, n);
                                result.append(cur);
                            }
                        }
                    } else {
                        state = State::End;
                    }
                }
                State::Ident => {
                    while ident_stack.len() > 1 {
                        if let Some(ident) = ident_stack.pop_front() {
                            result.append(ident);
                        }
                    }
                    state = State::IdentTilde;
                }
                State::IdentTilde => {
                    let cur = iter.next();
                    if let Some(cur) = cur {
                        if let TokenTree::Ident(ident_inner) = cur {
                            ident_stack.push_back(ident_inner.clone());
                            if ident_inner.eq(&self.ident) {
                                state = State::IdentTildeN;
                            } else {
                                // error
                                state = State::Start;
                            }
                        } else {
                            // error
                            state = State::Start;
                        }
                    } else {
                        state = State::End;
                    }
                }
                State::IdentTildeN => {
                    // look ahead
                    let cur = iter.next();
                    if let Some(cur) = cur {
                        if let TokenTree::Punct(punct) = &cur {
                            if punct.as_char().eq(&'~') {
                                state = State::IdentTildeNTilde;
                                continue;
                            }
                        }
                        self.join_and_replace_ident_stack(&mut ident_stack, n);
                        result.append(ident_stack.pop_back().unwrap());
                        look_ahead_stack.push(cur);
                        state = State::Start;
                    } else {
                        state = State::End;
                    }
                }

                State::IdentTildeNTilde => {
                    let cur = iter.next();
                    if let Some(cur) = cur {
                        match cur {
                            TokenTree::Ident(ident_inner) => {
                                ident_stack.push_back(ident_inner.clone());
                                state = State::IdentTildeNTildeIdent;
                            }
                            _ => {
                                // error
                                state = State::Start;
                            }
                        }
                    } else {
                        state = State::End;
                    }
                }

                State::IdentTildeNTildeIdent => {
                    self.join_and_replace_ident_stack(&mut ident_stack, n);
                    result.append(ident_stack.pop_back().unwrap());
                    state = State::Start;
                }
                State::End => {
                    self.flush_ident_stack(&mut result, &mut ident_stack, n);
                    break;
                }
            }
        }

        result
    }

    fn flush_ident_stack(
        &self,
        result: &mut proc_macro2::TokenStream,
        stack: &mut VecDeque<Ident>,
        n: i32,
    ) {
        if stack.is_empty() {
            return;
        }

        while let Some(ident_inner) = stack.pop_front() {
            if ident_inner.eq(&self.ident) {
                result.append(Literal::i32_unsuffixed(n));
            } else {
                result.append(ident_inner.clone());
            }
        }

        stack.clear();
    }
    fn join_and_replace_ident_stack(&self, stack: &mut VecDeque<Ident>, n: i32) {
        if stack.is_empty() {
            return;
        }
        let first_span = stack.front().unwrap().span();
        let ident_string = stack
            .iter()
            .map(|ident_inner| {
                if ident_inner.eq(&self.ident) {
                    n.to_string()
                } else {
                    ident_inner.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("");

        stack.clear();
        stack.push_back(Ident::new(&ident_string, first_span));
        debug_assert_eq!(stack.len(), 1);
    }
}
