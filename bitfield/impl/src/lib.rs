use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, Item};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let ast = parse_macro_input!(input as Item);

    ast.to_token_stream().into()
}
