# glium_derive
Custom derive for glium Vertex

## Example
```rust
extern crate glium;
#[macro_use]
extern crate glium_derive;

#[derive(Clone, Copy, Vertex)]
struct MyVertex {
    #[glium(attr = "a_pos")
    pos: [f32; 3],
    #[glium(attr = "a_uv")
    uv: [f32; 2],
    #[glium(attr = "a_color", normalize)
    color: u32,
}
```
