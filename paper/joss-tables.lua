-- Quarto resolves cross-references before rendering the Markdown used by Inara.
-- Carry caption metadata and link targets into that Markdown.
quarto._quarto.ast.add_renderer("FloatRefTarget", function(float)
  return FORMAT == "markdown" and float.type == "Table" and float.content.t == "Table"
end, function(float)
  local content = float.content
  content.identifier = float.identifier
  content.caption.long = quarto.utils.as_blocks(float.caption_long)

  local colwidths = float.attributes["tbl-colwidths"]
  if colwidths then
    local widths = quarto.json.decode(colwidths)
    for i, width in ipairs(widths) do
      content.colspecs[i] = {content.colspecs[i][1], width / 100}
    end
    -- Pandoc encodes widths as column padding in Markdown tables.
    local markdown = pandoc.write(pandoc.Pandoc({content}), "markdown", {columns = 255})
    return pandoc.RawBlock("markdown", markdown)
  end
  return content
end)

quarto._quarto.ast.add_renderer("FloatRefTarget", function(float)
  return (FORMAT == "markdown" or FORMAT == "latex") and float.type == "Table"
    and float.classes:includes("sideways-table")
end, function(float)
  local content = quarto.utils.as_blocks(float.content)
  if #content ~= 1 or content[1].t ~= "RawBlock" or content[1].format ~= "latex" then
    error("A sideways table must contain a single raw LaTeX tabular block.")
  end

  local caption = quarto.utils.as_blocks(float.caption_long)
  local title = pandoc.write(pandoc.Pandoc(caption), "latex"):gsub("%s+$", "")
  local environment = FORMAT == "markdown" and "table" or "sidewaystable"
  local latex_table = table.concat({
    "\\begin{" .. environment .. "}[p]",
    "\\centering",
    "\\small",
    "\\caption{" .. title .. "}",
    "\\label{" .. float.identifier .. "}",
    content[1].text,
    "\\end{" .. environment .. "}",
  }, "\n")

  if FORMAT == "latex" then
    return pandoc.RawBlock("latex", latex_table)
  end

  -- Inara reads the standard table's caption and label for JATS. Separate
  -- raw LaTeX fences apply rotation to this table in the PDF build.
  return pandoc.RawBlock("markdown", table.concat({
    "```{=latex}",
    "\\begingroup",
    "\\let\\table\\sidewaystable",
    "\\let\\endtable\\endsidewaystable",
    "```",
    "",
    latex_table,
    "",
    "```{=latex}",
    "\\endgroup",
    "```",
  }, "\n"))
end)

return {}
