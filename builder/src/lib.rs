use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, token::Type};
// #[derive(std::fmt::Debug)]
// pub enum BuilderError {
//     NoneField(std::string::String),
// }

// impl std::fmt::Display for BuilderError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "builder error")?;
//         match self {
//             NoneField(field) => {
//                 write!(f, "field is none: {}", field)
//             }
//         }
//     }
// }

// impl std::error::Error for BuilderError {}
#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_ident = &ast.ident;
    let struct_ident_literal = ast.ident.to_string();
    let builder_ident_literal = format!("{}Builder", struct_ident_literal);
    let builder_ident = Ident::new(&builder_ident_literal, ast.span());

    let mut builder_fields = quote! {};
    let mut builder_implementation = quote! {};
    let mut struct_check_none = quote! {};
    let mut struct_initialize = quote! {};
    let mut builder_initialize = quote! {};

    match &ast.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            for field in fields {
                let ty = field.ty.to_token_stream();
                let field_ident = field.ident.as_ref().unwrap();
                let option_generic_ty = match &field.ty {
                    syn::Type::Path(syn::TypePath { path, .. }) => {
                        let segment_last = path.segments.last().unwrap();
                        if segment_last.ident.eq("Option".into()) {
                            match &segment_last.arguments {
                                syn::PathArguments::AngleBracketed(
                                    syn::AngleBracketedGenericArguments { args, .. },
                                ) => {
                                    debug_assert_eq!(args.len(), 1);
                                    let generic_ty = args.last().unwrap();
                                    match generic_ty {
                                        syn::GenericArgument::Type(ty) => Some(ty),
                                        _ => None,
                                    }
                                }
                                _ => None,
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                builder_initialize.extend(quote! {
                    #field_ident: std::option::Option::None,
                });
                match option_generic_ty {
                    Some(option_generic_ty) => {
                        let option_generic_ty = option_generic_ty.to_token_stream();
                        builder_fields.extend(quote! {
                            #field_ident: #ty,
                        });
                        builder_implementation.extend(quote! {
                            fn #field_ident(&mut self, #field_ident: #option_generic_ty) -> &mut Self {
                                self.#field_ident = std::option::Option::Some(#field_ident);
                                self
                            }
                        });
                        struct_check_none.extend(quote! {});
                        struct_initialize.extend(quote! {
                            #field_ident: self.#field_ident.take(),
                        });
                    }
                    None => {
                        builder_fields.extend(quote! {
                            #field_ident: std::option::Option<#ty>,
                        });
                        builder_implementation.extend(quote! {
                        fn #field_ident(&mut self, #field_ident: #ty) -> &mut Self {
                            self.#field_ident = std::option::Option::Some(#field_ident);
                            self
                        }
                        });
                        struct_check_none.extend(quote! {
                            if self.#field_ident.is_none() {
                                return std::result::Result::Err(format!("field is none: {}", stringify!(#field_ident)).into()); 
                            }
                        });
                        struct_initialize.extend(quote! {
                            #field_ident: self.#field_ident.take().unwrap(),
                        });
                    }
                }
            }
        }
        _ => unimplemented!(),
    };
    quote! {
        pub struct #builder_ident {
            #builder_fields
        }

        impl #builder_ident {
            #builder_implementation

            pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                #struct_check_none
                std::result::Result::Ok(
                    #struct_ident {
                        #struct_initialize
                    }
                )
            }
        }

        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #builder_initialize
                }
            }
        }

    }
    .into()
}
