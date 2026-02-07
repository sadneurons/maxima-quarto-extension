-- Maxima Lua filter for Quarto
-- Provides additional processing for Maxima code blocks

function CodeBlock(block)
  -- Check if this is a maxima code block
  if block.classes[1] == "maxima" then
    -- Add any custom processing here
    -- For now, just pass through
    return block
  end
end

function Div(div)
  -- Process divs with maxima class
  if div.classes[1] == "maxima" then
    return div
  end
end

return {
  {CodeBlock = CodeBlock},
  {Div = Div}
}
