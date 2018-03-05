Rainbow-ui
---
UI/game/graphics framework evolved while developing [vimtris](https://github.com/codewaterlabs/vimtris).
The state of it is not too bad in some areas, but there are many areas that are just scetched/explored towards the needs in vimtris, only the curious should use it :)

The framework has the concept of nodes, which are the parts of a scene tree, similarly to html dom. Some nodes are just layout nodes, while others draw to the screen (and in the future should get events like onclick/mouseover). These nodes are connected to graphics programming elements like shaders, uniforms, vertices and textures. Each node can be associated with a program that draws it's content in it's layed out position.

Uniform objects can be passed to nodes, and when updated, the required nodes will be redrawn, the same goes for textures (and nodes providing textures to other nodes). Nodes do not redraw each frame unless there is change in input. When a node needs update, and this node is transparent or partial drawing (not full quad of it's layout area) the system also draws required nodes as it knows it from the layout.

There is a simple animation system that can update uniforms.
There is some code to provide lighting to nodes.
There is code for font rendering through sdf (signed distance field) fonts (see [font-gen](https://github.com/codewaterlabs/font-gen) for how to generate font files).
There is some layout system for text in FontText.re where you can use blocks and styled fragments to lay out and color text.

To continue to work on the system I would like to create documentation and clarify api's, test more, try to figure out what makes in various apis. This is early steps.

As time goes by I hope the library/framework evolves to be a robust system for ui's and games (possibly better suited for some more layoutish games), and also presentation material like webpages.