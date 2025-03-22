use std::fmt::Display;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse_macro_input, spanned::Spanned, Attribute, Expr, ExprMatch, Item, ItemEnum,
    ItemFn, Pat, PatIdent, PatTupleStruct, Stmt,
};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as SortedArgs);
    let ast = parse_macro_input!(input as Item);

    let result = match expand(&args, &ast) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    };

    result
}

#[derive(Clone)]
struct SortedArgs {
    attrs: Vec<Attribute>,
}

impl Parse for SortedArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
        })
    }
}

impl ToTokens for SortedArgs {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for attr in self.attrs.iter() {
            attr.to_tokens(tokens);
        }
    }
}

fn expand(args: &SortedArgs, ast: &Item) -> syn::Result<proc_macro2::TokenStream> {
    check_enum(ast, args.span())?;
    check_order_item(ast)?;
    Ok(quote! {
        #ast
    })
}

fn check_enum(ast: &Item, span: Span) -> syn::Result<()> {
    if !matches!(ast, Item::Enum(_)) {
        return Err(syn::Error::new(span, "expected enum or match expression"));
    }

    Ok(())
}

fn check_order_item(ast: &Item) -> syn::Result<()> {
    if let Item::Enum(ItemEnum { variants, .. }) = ast {
        let idents = variants
            .iter()
            .map(|variant| variant.ident.clone())
            .collect::<Vec<_>>();

        check_order(idents)?;
    }

    Ok(())
}

fn check_order(lits: Vec<impl Ord + Display + Spanned>) -> syn::Result<()> {
    let mut iter = lits.iter().enumerate();
    let (_, mut prev) = iter.next().unwrap();
    if lits.len() <= 1 {
        return Ok(());
    }

    for (mut index, cur) in iter {
        if cur.lt(&prev) {
            loop {
                index -= 1;
                let prev = &lits[index];
                if prev.lt(&cur) {
                    let next = &lits[index + 1];
                    return Err(syn::Error::new(
                        cur.span(),
                        format!("{} should sort before {}", cur, next),
                    ));
                }

                if index == 0 {
                    let next = &lits[0];
                    return Err(syn::Error::new(
                        cur.span(),
                        format!("{} should sort before {}", cur, next),
                    ));
                }
            }
        }
        prev = cur;
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut ast = parse_macro_input!(input as Item);

    match expand_check(args.clone(), &mut ast) {
        Ok(_) => TokenStream::from(ast.to_token_stream()),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_check(args: TokenStream, ast: &mut Item) -> syn::Result<()> {
    if let Item::Fn(ItemFn { block, .. }) = ast {
        resolve_sorted_attr(&mut block.stmts)
    } else {
        Err(syn::Error::new_spanned(
            proc_macro2::TokenStream::from(args),
            "expected fn",
        ))
    }
}

fn resolve_sorted_attr(stmts: &mut Vec<Stmt>) -> syn::Result<()> {
    for stmt in stmts.iter_mut() {
        match stmt {
            Stmt::Expr(expr, ..) => {
                if let Expr::Match(expr_match) = expr {
                    resolve_match_expr(expr_match);
                    sorted_order_match(expr_match)?;
                }
            }
            _ => (),
        }
    }

    Ok(())
}

fn sorted_order_match(expr_match: &mut ExprMatch) -> syn::Result<()> {
    let mut paths = vec![];
    let mut flag = None;

    for arm in &expr_match.arms {
        if let Pat::TupleStruct(PatTupleStruct { path, .. }) = &arm.pat {
            // debug_assert_eq!(path.segments.len(), 1);

            let path = path
                .segments
                .to_token_stream()
                .to_string()
                .split_whitespace()
                .collect::<Vec<_>>()
                .join("")
                .to_string();
            paths.push(path);
            // println!("path: {}", path.get_ident().cloned().unwrap());
        } else if matches!(&arm.pat, Pat::Wild(_)) {
            flag = Some(arm.pat.span());
        } else if let Pat::Ident(PatIdent { ident, .. }) = &arm.pat {
            paths.push(ident.to_string());
        } else {
            return Err(syn::Error::new_spanned(
                arm.pat.to_token_stream(),
                "unsupported by #[sorted]",
            ));
        }
    }

    if flag.is_some() {
        if !matches!(&expr_match.arms.last().unwrap().pat, Pat::Wild(_)) {
            return Err(syn::Error::new(
                flag.unwrap(),
                "wild match should be the last",
            ));
        }
    }

    check_order(paths)?;
    Ok(())
}

fn resolve_match_expr(expr_match: &mut ExprMatch) {
    expr_match.attrs = expr_match
        .attrs
        .iter()
        .cloned()
        .filter(|attr| !attr.path().is_ident("sorted"))
        .collect::<Vec<_>>();
}
