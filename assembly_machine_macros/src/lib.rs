#![feature(extract_if)]

use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse, spanned::Spanned, ItemEnum, ItemStruct};

const START: &str = "span_start";
const END: &str = "span_end";

#[proc_macro_attribute]
pub fn enum_from(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse::<ItemEnum>(item).unwrap();
    let name = &input.ident;
    let variant_names = input.variants.iter().map(|v| &v.ident);
    let variant_types = input.variants.iter().map(|v| &v.fields.iter().next().unwrap().ty);

    // Check if the type is a Box and if so take the inner type in the From impl and wrap it.

    let mut output: proc_macro2::TokenStream = input.to_token_stream().into();

    for (var, ty) in variant_names.zip(variant_types) {
        let (ty, is_box) = if let syn::Type::Path(p) = ty {
            if p.path.segments.iter().next().unwrap().ident == "Box" {
                let inner = match p.path.segments.iter().next().unwrap().arguments {
                    syn::PathArguments::AngleBracketed(ref args) => args.args.iter().next().unwrap(),
                    _ => unreachable!()
                };

                (inner.into_token_stream(), true)
            } else {
                (ty.into_token_stream(), false)
            }
        } else {
            (ty.into_token_stream(), false)
        };
        
        let from = if is_box {
            quote! {
                impl From<#ty> for #name {
                    fn from(v: #ty) -> Self {
                        Self::#var(Box::new(v))
                    }
                }
            }
        } else {
            quote! {
                impl From<#ty> for #name {
                    fn from(v: #ty) -> Self {
                        Self::#var(v)
                    }
                }
            }
        };

        output.extend(from);
    }

    output.into()
}

#[proc_macro_derive(SpanData, attributes(span_start, span_end))]
pub fn derive_span_data(item: TokenStream) -> TokenStream {
    if let Ok(input) = parse::<ItemEnum>(item.clone()) {
        handle_enum(input)
    } else if let Ok(input) = parse::<ItemStruct>(item) {
        handle_struct(input)
    } else {
        panic!("Macro only valid on Enum or Struct.")
    }
}

fn handle_enum(input: ItemEnum) -> TokenStream {
    let name = &input.ident;
    let variants = input.variants.iter().map(|v| (&v.ident, v.span()));

    let mut matches: Vec<proc_macro2::TokenStream> = Vec::with_capacity(variants.len());

    for (var, span) in variants {
        matches.push(quote_spanned!(span=> Self::#var(v) => v.get_span()));
    }

    let output = quote! {
        impl SpanData for #name {
            fn get_span(&self) -> Span {
                match self {
                   #(#matches),*
                }
            }
        }
    };

    output.into()
}

fn handle_struct(input: ItemStruct) -> TokenStream {
    assert!(!input.fields.is_empty(), "Expected Struct to have at least one item");

    let name = &input.ident;

    if input.fields.iter().next().unwrap().ident.is_none() {
        let count = input.fields.len() - 1;
        
        return quote! {
            impl SpanData for #name {
                fn get_span(&self) -> Span {
                    self.#count.get_span()
                }
            }
        }.into()
    }

    let (mut l, mut r) = (None, None);
    for field in input.fields.iter() {
        if  let Some(attr) = field.attrs.iter().find(|a| a.path().is_ident(START)) {
            if l.is_some() { return syn::Error::new(attr.span(), "only allowed one start attribute").to_compile_error().into() }
            l = Some(field);
        } else if let Some(attr) = field.attrs.iter().find(|a| a.path().is_ident(END)) {
            if r.is_some() { return syn::Error::new(attr.span(), "only allowed one end attribute").to_compile_error().into() }
            r = Some(field);
        }

        if l.is_some() && r.is_some() {
            break;
        }
    }

    if let (Some(l), Some(r)) = (l, r) {
        let (l_id, r_id) = (l.ident.as_ref().unwrap(), r.ident.as_ref().unwrap());

        if let syn::Type::Path(p) = &r.ty {
            if p.path.segments.iter().next().unwrap().ident == "Option" {
                return quote! {
                    impl SpanData for #name {
                        fn get_span(&self) -> Span {
                            Span::expand(self.#l_id.get_span(), &match &self.#r_id.as_ref() {
                                Some(#r_id) => #r_id.get_span(),
                                None => self.#l_id.get_span()
                            })
                        }
                    }
                }.into()
            }
        }

        return quote! {
            impl SpanData for #name {
                fn get_span(&self) -> Span {
                    Span::expand(self.#l_id.get_span(), &self.#r_id.get_span())
                }
            }
        }.into()
    }

    let field = &input.fields.iter().next().unwrap().ident.as_ref().unwrap();

    let output = quote! {
        impl SpanData for #name {
            fn get_span(&self) -> Span {
                self.#field.get_span()
            }
        } 
    };

    output.into()
}
