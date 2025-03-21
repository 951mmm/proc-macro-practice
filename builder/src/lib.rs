use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Field, LitStr, Type};
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
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match expand(ast) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn expand(ast: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &ast.ident;
    let struct_ident_literal = ast.ident.to_string();
    let builder_ident_literal = format!("{}Builder", struct_ident_literal);
    let builder_ident = Ident::new(&builder_ident_literal, ast.span());

    let mut builder_fields = quote! {};
    let mut builder_implementations = quote! {};
    let mut struct_check_nones = quote! {};
    let mut builder_initializes = quote! {};
    let mut struct_initializes = quote! {};

    let result = match &ast.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            for field in fields {
                let field_ident = get_field_ident(field);
                let field_ty = &field.ty;
                let option_inner_ty = get_generic_inner_ty(field, "Option");
                let each_fn_literal = get_attr_value(field)?;
                let vec_inner_ty = get_generic_inner_ty(field, "Vec");

                if each_fn_literal.is_some() && vec_inner_ty.is_none() {
                    return Err(syn::Error::new(
                        field.span(),
                        "type attred with `#[builder (each = \"...\")]` should be Vec<_>",
                    ));
                }

                builder_fields.extend(get_builder_field(
                    field_ident,
                    field_ty,
                    option_inner_ty,
                    &each_fn_literal,
                ));

                builder_implementations.extend(get_builder_implementation(
                    field_ident,
                    field_ty,
                    option_inner_ty,
                    &each_fn_literal,
                    vec_inner_ty,
                ));

                struct_check_nones.extend(get_struct_check_none(
                    field_ident,
                    option_inner_ty,
                    &each_fn_literal,
                ));

                builder_initializes.extend(get_builder_initialize(field_ident, &each_fn_literal));

                struct_initializes.extend(get_struct_initialize(
                    field_ident,
                    option_inner_ty,
                    &each_fn_literal,
                ));
            }
            quote! {
                pub struct #builder_ident {
                    #builder_fields
                }

                impl #builder_ident {
                    #builder_implementations

                    pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                        #struct_check_nones
                        std::result::Result::Ok(
                            #struct_ident {
                                #struct_initializes
                            }
                        )
                    }
                }

                impl #struct_ident {
                    pub fn builder() -> #builder_ident {
                        #builder_ident {
                            #builder_initializes
                        }
                    }
                }

            }
        }
        _ => quote! {},
    };

    Ok(result)
}

fn get_builder_field(
    field_ident: &Ident,
    field_ty: &Type,
    option_inner_ty: Option<&Type>,
    each_fn_literal: &Option<String>,
) -> proc_macro2::TokenStream {
    if each_fn_literal.is_some() {
        quote! {
            #field_ident: #field_ty,
        }
    } else if let Some(ty) = option_inner_ty {
        quote! {
            #field_ident: std::option::Option<#ty>,
        }
    } else {
        quote! {
            #field_ident: std::option::Option<#field_ty>,
        }
    }
}

fn get_builder_implementation(
    field_ident: &Ident,
    field_ty: &Type,
    option_inner_ty: Option<&Type>,
    each_fn_literal: &Option<String>,
    vec_inner_ty: Option<&Type>,
) -> proc_macro2::TokenStream {
    let (fn_ident, assign_expr, arg_ty) = match each_fn_literal {
        Some(fn_literal) => {
            let vec_inner_ty = vec_inner_ty.unwrap();
            let fn_ident = Ident::new(fn_literal, field_ident.span());
            let assign_expr = quote! {
                self.#field_ident.push(#fn_ident)
            };
            (fn_ident, assign_expr, vec_inner_ty)
        }
        None => (
            field_ident.clone(),
            quote! {self.#field_ident = std::option::Option::Some(#field_ident)},
            field_ty,
        ),
    };

    match option_inner_ty {
        Some(ty) => {
            quote! {
                fn #fn_ident(&mut self, #fn_ident: #ty) -> &mut Self {
                    #assign_expr;
                    self
                }
            }
        }
        None => {
            quote! {
                fn #fn_ident(&mut self, #fn_ident: #arg_ty) -> &mut Self {
                    #assign_expr;
                    self
                }
            }
        }
    }
}

fn get_struct_check_none(
    field_ident: &Ident,
    option_inner_ty: Option<&Type>,
    each_fn_literal: &Option<String>,
) -> proc_macro2::TokenStream {
    match option_inner_ty {
        Some(_) => quote! {},
        None => match each_fn_literal {
            Some(_) => quote! {},
            None => quote! {
                if self.#field_ident.is_none() {
                    return std::result::Result::Err(format!("field is none: {}", stringify!(#field_ident)).into());
                }
            },
        },
    }
}

fn get_builder_initialize(
    field_ident: &Ident,
    each_fn_literal: &Option<String>,
) -> proc_macro2::TokenStream {
    match each_fn_literal {
        Some(_) => {
            quote! {
                #field_ident: vec![],
            }
        }
        None => {
            quote! {
                #field_ident: std::option::Option::None,
            }
        }
    }
}

fn get_struct_initialize(
    field_ident: &Ident,
    option_inner_ty: Option<&Type>,
    each_fn_literal: &Option<String>,
) -> proc_macro2::TokenStream {
    if each_fn_literal.is_some() {
        quote! {
            #field_ident: self.#field_ident.clone(),
        }
    } else if option_inner_ty.is_some() {
        quote! {
            #field_ident: self.#field_ident.take(),
        }
    } else {
        quote! {
            #field_ident: self.#field_ident.take().unwrap(),
        }
    }
}

fn get_generic_inner_ty<'a>(field: &'a Field, outer: &str) -> Option<&'a Type> {
    match &field.ty {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            let segment_last = path.segments.last().unwrap();
            if segment_last.ident.eq(outer) {
                match &segment_last.arguments {
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args,
                        ..
                    }) => {
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
    }
}

fn get_attr_value(field: &Field) -> syn::Result<Option<String>> {
    let mut each_fn_literal = None;
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each") {
                    let value = meta.value()?;
                    let string_literal = value.parse::<LitStr>()?;
                    each_fn_literal = Some(string_literal.value());
                    return Ok(());
                }
                Err(meta.error("expected `builder(each = \"...\")`"))
            })?
        }
    }
    Ok(each_fn_literal)
}

fn get_field_ident(field: &Field) -> &Ident {
    field.ident.as_ref().unwrap()
}
