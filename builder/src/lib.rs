extern crate proc_macro;
extern crate quote;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn is_optional(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath { path: p, .. }) => {
            p.segments.len() == 1 && p.segments[0].ident == "Option"
        }
        _ => false,
    }
}

fn builder_path_to_match() -> syn::Path {
    syn::parse_str("builder").expect("builder is a valid path")
}

fn each_path_to_match() -> syn::Path {
    syn::parse_str("each").expect("builder is a valid path")
}

fn extract_attribute_value(
    attr: &syn::Attribute,
    name: &syn::Path,
) -> Option<(String, proc_macro2::Span)> {
    match attr.parse_meta() {
        Ok(syn::Meta::List(items)) => {
            for item in items.nested.iter() {
                match item {
                    syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                        path,
                        lit: syn::Lit::Str(lit),
                        ..
                    })) => {
                        if path == name {
                            return Some((lit.value(), lit.span()));
                        }
                    }
                    _ => {}
                }
            }
            return None;
        }
        _ => None,
    }
}

fn inner_type<'a>(ty: &'a syn::Type) -> Option<&'a syn::Type> {
    match ty {
        syn::Type::Path(syn::TypePath { path: p, .. }) => {
            if p.segments.len() != 1 {
                return None;
            }
            match p.segments.iter().next().unwrap().arguments {
                syn::PathArguments::AngleBracketed(ref inner) => {
                    if inner.args.len() != 1 {
                        None
                    } else {
                        let inner = inner.args.first().unwrap();
                        match inner {
                            syn::GenericArgument::Type(ref t) => Some(t),
                            _ => None,
                        }
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;
    let builder = syn::Ident::new(&format!("{}Builder", &name), name.span());

    let named_ast_fields = match ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named,
        _ => unimplemented!(),
    };

    let quoted_fields = named_ast_fields.iter().map(|f| {
        let ident = &f.ident;
        let field_type = &f.ty;
        let builder_path_repr = builder_path_to_match();
        let meta_func = f.attrs.iter().find(|attr| attr.path == builder_path_repr);
        if is_optional(&field_type) || !meta_func.is_none() {
            quote! { #ident: #field_type }
        } else {
            quote! { #ident: ::std::option::Option<#field_type> }
        }
    });

    let setters = named_ast_fields.iter().map(|f| {
        let ident = &f.ident;
        let field_type = &f.ty;
        let builder_path_repr = builder_path_to_match();
        let meta_func = f.attrs.iter().find(|attr| attr.path == builder_path_repr);
        // I'm sort of confused about how `Option<Vec<T>>` would work with `[#builder(each = "..")]` semantics...
        // Therefore, I'll just assume that optional and each are mutually exclusive
        if is_optional(&field_type) {
            let inner_ty = inner_type(field_type);
            quote! {
                fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            match meta_func {
                Some(a) => match extract_attribute_value(a, &each_path_to_match()) {
                    Some((val, span)) => {
                        // This is a hacky way to see if we're about to define a function defined by builder :(
                        // If I was smart and created the functions in one map this wouldn't be need to be repetitive...
                        if &Some(syn::Ident::new(&val, span)) == ident {
                            return quote!();
                        }
                        quote!(
                            pub fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                                self.#ident = #ident;
                                self
                            }
                        )
                    }
                    // A big oof, since this was a last minute fix I didn't feel like going back and fixing things right
                    // So instead we parse meta again :tada:
                    None => match a.parse_meta() {
                        Ok(meta_val) => {
                            // Lots of trial and errors about what to put here
                            return syn::Error::new_spanned(
                                meta_val,
                                "expected `builder(each = \"...\")`",
                            )
                            .to_compile_error();
                        }
                        _ => {
                            return syn::Error::new_spanned(
                                meta_func,
                                "expected `builder(each = \"...\")`",
                            )
                            .to_compile_error()
                        }
                    },
                },
                None => quote! {
                    fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                },
            }
        }
    });

    let appenders = named_ast_fields.iter().map(|f| {
        let ident = &f.ident;
        let field_type = &f.ty;
        let builder_path_repr = builder_path_to_match();
        let meta_func = f.attrs.iter().find(|attr| attr.path == builder_path_repr);
        match meta_func {
            Some(a) => match extract_attribute_value(a, &each_path_to_match()) {
                Some((val, span)) => {
                    let each_ident = syn::Ident::new(&val, span);
                    let inner_ty = inner_type(&field_type).unwrap();
                    quote!(
                        pub fn #each_ident(&mut self, #each_ident: #inner_ty) -> &mut Self {
                            self.#ident.push(#each_ident);
                            self
                        }
                    )
                }
                None => quote!(),
            },
            _ => quote!(),
        }
    });

    let empty_fields = named_ast_fields.iter().map(|f| {
        let ident = &f.ident;
        let builder_path_repr = builder_path_to_match();
        let meta_func = f.attrs.iter().find(|attr| attr.path == builder_path_repr);
        if meta_func.is_none() {
            quote! { #ident: ::std::option::Option::None }
        } else {
            quote! { #ident: ::std::vec::Vec::new() }
        }
    });

    let set_fields = named_ast_fields.iter().map(|f| {
        let ident = &f.ident;
        let field_type = &f.ty;
        let builder_path_repr = builder_path_to_match();
        let meta_func = f.attrs.iter().find(|attr| attr.path == builder_path_repr);
        if is_optional(field_type) {
            quote! {
                #ident: self.#ident.clone()
            }
        } else if meta_func.is_none() {
            quote! {
                #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is not set, but is required"))?
            }
        } else {
            quote! {
                #ident: self.#ident.clone()
            }
        }
    });

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#empty_fields),*
                }
            }
        }

        pub struct #builder {
            #(#quoted_fields,)*
        }

        impl #builder {
            #(#setters)*

            #(#appenders)*

            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                return Ok(#name {
                    #(#set_fields),*
                });
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}
