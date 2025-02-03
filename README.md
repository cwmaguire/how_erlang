# how_erlang
A mind map tool with links

## Goal
layout links spatially using drag and drop.

## Use
- Double-click on empty space to create a new box
- Double-click on a box to edit
- Click "save" in the edit view to save changes
- Click "cancel" in the edit view to discard changes
- Click "copy" to copy a box
- Click "delete" to delete a box
- Mouse over the corners of boxes to reveal drag handles
- Click and drag the drag handles to resize a box

## Build

Install `erlang` and `rebar3`. Then, run:

- `rebar3 get-deps`
- `rebar3 compile`
- `rebar3 shell`

You can then navigate to `http://localhost:8080` in your browser to see the app.

## Details
- Cowboy web server in Erlang running in JSON provides and accepts a JSON file with URL details: name, desc, coordinates, etc.
- Web page fetches URLs from server and saves them back.
- No JavaScript frameworks are used. We're just going in raw.
