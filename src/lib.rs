//! A custom derive implementation for Glium Vertex and Uniforms traits
//!
//! ## Example
//! ```rust
//! extern crate glium;
//! #[macro_use]
//! extern crate glium_derive;
//!
//! #[derive(Clone, Copy, Vertex)]
//! struct MyVertex {
//!     #[glium(attr = "a_pos")]
//!     pos: [f32; 3],
//!     #[glium(attr = "a_uv")]
//!     uv: [f32; 2],
//!     #[glium(attr = "a_color", normalize)]
//!     color: u32,
//!     a_custom: u32, // the attribute can be omitted
//! }
//!
//! //! #[derive(Clone, Copy, Uniforms)]
//! struct MyUniforms {
//!     #[glium(attr = "a_view")]
//!     view: [[f32; 4]; 4],
//!     #[glium(attr = "a_color")]
//!     color: [f32; 4],
//!     a_custom: u32, // the attribute can be omitted
//! }
//! ```

#![recursion_limit = "128"]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{spanned::Spanned, Field};
use syn::{
    Attribute, Data, DataStruct, DeriveInput, Fields, Meta, MetaList, MetaNameValue, NestedMeta,
};

#[proc_macro_derive(Vertex, attributes(glium))]
pub fn glium_vertex_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_glium_vertex_derive(&ast)
}

fn impl_glium_vertex_derive(ast: &DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;
    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = match ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => &fields.named,
        _ => {
            panic!("#[derive(Vertex)] only defined for structs.");
        }
    };

    let bindings = fields.iter().map(|field| {
        let attrs = get_attributes(field);
        let field_name = &field.ident;
        let vertex_attr_lit = format!("{}", field_name.as_ref().unwrap());
        let mut vertex_attr_name = quote!(#vertex_attr_lit);
        let mut vertex_location: i32 = -1;
        let mut normalize = false;

        for meta in attrs {
            match meta {
                NestedMeta::Meta(Meta::NameValue(MetaNameValue { ref path, ref lit, .. })) => {
                    let ident = path.get_ident().expect("Expected ident, got path");

                    if ident == "attr" {
                        vertex_attr_name = quote!(#lit);
                    } else if ident == "location" {
                        vertex_location = format!("{}", quote!(#lit)).parse().unwrap();
                    } else {
                        panic!("Unknown field attribute {}", ident);
                    }
                },
                NestedMeta::Meta(Meta::Path(ref path)) => {
                    let ident = path.get_ident().expect("Expected ident, got path");

                    if ident == "normalize" {
                        normalize = true;
                    } else {
                        panic!("Unknown field attribute {}", ident);
                    }
                },
                _ => (),
            }
        };

        quote_spanned! {field.span()=>
            (
                std::borrow::Cow::Borrowed(#vertex_attr_name),
                {
                    let offset: usize = {
                        let uninit = core::mem::MaybeUninit::<#struct_name>::uninit();
                        let uninit_ptr = uninit.as_ptr();
                        let field_ptr = unsafe { &(*uninit_ptr).#field_name as *const _ };

                        unsafe { (field_ptr as *const u8).offset_from(uninit_ptr as *const u8) as usize }
                    };
                    offset
                },
                #vertex_location,
                {
                    const fn attr_type_of_val<T: ::glium::vertex::Attribute>(_: &T) -> ::glium::vertex::AttributeType {
                        <T as ::glium::vertex::Attribute>::TYPE
                    }

                    let uninit = core::mem::MaybeUninit::<#struct_name>::uninit();
                    let uninit_ptr = uninit.as_ptr();
                    let field_ref = unsafe { &(*uninit_ptr).#field_name };
                    attr_type_of_val(field_ref)
                },
                #normalize
            )
        }
    });

    let stream = quote! {
        impl #struct_name #ty_generics #where_clause {
            const BINDINGS: ::glium::vertex::VertexFormat = &[
                #(
                    #bindings,
                )*
            ];
        }

        impl #impl_generics ::glium::vertex::Vertex for #struct_name #ty_generics #where_clause {
            #[inline]
            fn build_bindings() -> ::glium::vertex::VertexFormat {
                Self::BINDINGS
            }
        }
    };

    stream.into()
}

#[proc_macro_derive(Uniforms, attributes(glium))]
pub fn glium_uniforms_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_glium_uniforms_derive(&ast)
}

fn impl_glium_uniforms_derive(ast: &DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;
    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = match ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => &fields.named,
        _ => {
            panic!("#[derive(Uniforms)] only defined for structs.");
        }
    };

    let bindings = fields.iter().map(|field| {
        let attrs = get_attributes(field);

        let field_name = &field.ident;
        let uniform_lit = format!("{}", field_name.as_ref().unwrap());
        let mut uniform_name = quote!(#uniform_lit);

        for meta in attrs {
            match meta {
                NestedMeta::Meta(Meta::NameValue(MetaNameValue { ref path, ref lit, .. })) => {
                    let ident = path.get_ident().expect("Expected ident, got path");

                    if ident == "attr" {
                        uniform_name = quote!(#lit);
                    } else {
                        panic!("Unknown field attribute {}", ident);
                    }
                },
                NestedMeta::Meta(Meta::Path(ref path)) => {
                    let ident = path.get_ident().expect("Expected ident, got path");
                    panic!("Unknown field attribute {}", ident);
                },
                _ => (),
            }
        };

        quote_spanned! {field.span()=>
            output(#uniform_name, ::glium::uniforms::AsUniformValue::as_uniform_value(&self.#field_name));
        }
    });

    let stream = quote! {
        impl #impl_generics ::glium::uniforms::Uniforms for #struct_name #ty_generics #where_clause {
            #[inline]
            fn visit_values<'a, F: FnMut(&str, ::glium::uniforms::UniformValue<'a>)>(&'a self, mut output: F) {
                #(
                    #bindings
                )*
            }
        }
    };

    stream.into()
}

fn get_attributes<'a>(field: &'a Field) -> impl Iterator<Item = NestedMeta> + 'a {
    field
        .attrs
        .iter()
        .flat_map(Attribute::parse_meta)
        .flat_map(|meta| {
            match meta {
                Meta::List(MetaList {
                    ref path,
                    ref nested,
                    ..
                }) => {
                    let ident = path.get_ident().expect("Expected ident, got path");

                    if ident == "glium" {
                        return nested.iter().cloned().collect();
                    }
                }
                _ => {}
            }

            Vec::new()
        })
}
