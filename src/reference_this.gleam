//// `reference_this` is a simple tool
//// that reads a folder of files (mainly .gleam) and copies the
//// content into a gleam file as strings. This is useful if you, for
//// instance, wish to build a custom website to display the examples/tutorials
//// for a gleam library.

import argv
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree.{type StringTree}
import glint
import simplifile

type Path =
  List(String)

type Reference {
  Reference(path: Path, module_doc: String, content: String)
}

fn path_from_string(s: String) -> Path {
  // FIXME: support windows paths '\'
  string.split(s, on: "/")
}

fn is_gleam_file(path: String) -> Bool {
  let path = path_from_string(path)
  {
    use filename <- result.try(list.last(path))
    let parts = string.split(filename, on: ".")
    use extension <- result.try(list.last(parts))
    Ok(extension == "gleam")
  }
  // if we fail to extract a file extension,
  // assume false
  |> result.unwrap(False)
}

// TODO: using a pretty-printer DSL instead would be nice :)
// I should take a look at 'glam' later. (Or at least use a string builder....)

const type_decl = "//// This module has been generated by the 'reference_this' tool.\n\n"
  <> "pub type Path = List(String)\n\n"
  <> "pub type Reference {\n"
  <> "  Reference(path: Path, module_doc: String, content: String)\n"
  <> "}\n"

fn escape_helper(chars: List(String), sb: StringTree) -> String {
  case chars {
    [] -> string_tree.to_string(sb)
    [c, ..cs] -> {
      case c {
        "\\" -> escape_helper(cs, string_tree.append(sb, "\\\\"))
        "\n" -> escape_helper(cs, string_tree.append(sb, "\\n"))
        "\"" -> escape_helper(cs, string_tree.append(sb, "\\\""))
        c -> escape_helper(cs, string_tree.append(sb, c))
      }
    }
  }
}

fn escape(s: String) -> String {
  escape_helper(string.to_graphemes(s), string_tree.new())
}

fn qoute(s: String) -> String {
  let s = escape(s)
  "\"" <> s <> "\""
}

fn serialize_path(path: Path) -> String {
  "[" <> path |> list.map(qoute) |> string.join(with: ", ") <> "]"
}

fn serialize_reference(reference: Reference) -> String {
  let Reference(path, module_doc, content) = reference
  "Reference("
  <> "path: "
  <> serialize_path(path)
  <> ", "
  <> "module_doc: "
  <> qoute(module_doc)
  <> ", "
  <> "content: "
  <> qoute(content)
  <> ")"
}

fn serialize_reference_list(references: List(Reference)) -> String {
  let indent = string.repeat(" ", 2)

  let references =
    references
    |> list.map(serialize_reference)
    |> string.join(",\n" <> indent)

  "pub const references: List(Reference) = [\n" <> indent <> references <> "\n]"
}

fn consume_module_doc(content: String) -> #(String, String) {
  // FIXME: we don't want to do this for non .gleam files
  // FIXME: this would fail on \r\n, is there really no split lines util?
  let lines = string.split(content, on: "\n")

  let prefixed_slashes = fn(s) { string.starts_with(s, "////") }
  let #(doc, rest) = list.split_while(lines, prefixed_slashes)

  // Remove the slashes from the doc comment
  let doc =
    list.fold(doc, "", fn(result, line) {
      let assert Ok(#(_, stripped_line)) = string.split_once(line, "////")
      result <> stripped_line <> "\n"
    })

  let rest = rest |> string.join(with: "\n") |> string.trim()

  #(doc, rest)
}

fn allow_all_ext() -> glint.Flag(Bool) {
  glint.bool_flag("allow-all-ext")
  |> glint.flag_default(False)
  |> glint.flag_help(
    "If we wish to allow all file extensions (not only .gleam)",
  )
}

fn run_cli() -> glint.Command(Nil) {
  // Setup CLI command
  use <- glint.command_help(
    "Reads all gleam files in <INPUT_DIRECTORY> and saves as strings in the gleam file <OUTPUT_FILE>.",
  )
  use directory_handle <- glint.named_arg("INPUT_DIRECTORY")
  use output_file_handle <- glint.named_arg("OUTPUT_FILE")
  use allow_all_ext <- glint.flag(allow_all_ext())
  use named, _args, flags <- glint.command()

  // resolve named args
  let directory = directory_handle(named)
  let output_file = output_file_handle(named)
  let assert Ok(allowed_all_ext) = allow_all_ext(flags)

  // Run the actual command and output potential errors
  case run(directory, output_file, allowed_all_ext) {
    Error(error) ->
      io.println_error("Error: " <> simplifile.describe_error(error))
    _ -> Nil
  }
}

fn run(directory: String, output_file: String, allowed_all_ext: Bool) {
  use files <- result.try(simplifile.get_files(directory))
  let files = list.filter(files, fn(f) { allowed_all_ext || is_gleam_file(f) })

  use files_content <- result.try(
    files
    |> list.map(simplifile.read)
    |> result.all(),
  )
  // Parse the module doc and split the file into a tuple (doc, rest )
  let files_content = files_content |> list.map(consume_module_doc)

  // Now let us generate references
  let references =
    list.zip(files, files_content)
    |> list.map(fn(file) {
      let #(path, #(doc, content)) = file
      let path = path_from_string(path)
      Reference(path, doc, content)
    })

  let output_file_content =
    type_decl <> "\n" <> serialize_reference_list(references)

  simplifile.write(to: output_file, contents: output_file_content)
}

pub fn main() {
  glint.new()
  |> glint.with_name("Reference")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: run_cli())
  |> glint.run(argv.load().arguments)
}