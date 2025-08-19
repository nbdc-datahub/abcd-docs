-- Custom citation filter for <@key> syntax to render full bibliography entries inline
local pandoc = require 'pandoc'

-- Cache for bibliography entries
local bib_cache = {}
local cited_keys = {}

-- Function to parse a simple bibtex entry and extract key information
function parse_bib_entry(entry_text, key)
  local result = {key = key}
  
  -- Extract authors
  local author = entry_text:match('author%s*=%s*{([^}]+)}') or entry_text:match('author%s*=%s*"([^"]+)"')
  if author then
    result.author = author
  end
  
  -- Extract year
  local year = entry_text:match('year%s*=%s*{([^}]+)}') or entry_text:match('year%s*=%s*"([^"]+)"') or entry_text:match('year%s*=%s*(%d+)')
  if year then
    result.year = year
  end
  
  -- Extract journal
  local journal = entry_text:match('journal%s*=%s*{([^}]+)}') or entry_text:match('journal%s*=%s*"([^"]+)"')
  if journal then
    result.journal = journal:gsub('{([^}]+)}', '%1') -- Remove extra braces
  end
  
  -- Extract volume
  local volume = entry_text:match('volume%s*=%s*{([^}]+)}') or entry_text:match('volume%s*=%s*"([^"]+)"') or entry_text:match('volume%s*=%s*(%d+)')
  if volume then
    result.volume = volume
  end
  
  -- Extract number/issue
  local number = entry_text:match('number%s*=%s*{([^}]+)}') or entry_text:match('number%s*=%s*"([^"]+)"') or entry_text:match('number%s*=%s*(%d+)')
  if number then
    result.number = number
  end
  
  -- Extract pages
  local pages = entry_text:match('pages%s*=%s*{([^}]+)}') or entry_text:match('pages%s*=%s*"([^"]+)"')
  if pages then
    result.pages = pages:gsub('%-%-', '–') -- Convert -- to en dash
  end
  
  -- Extract DOI
  local doi = entry_text:match('doi%s*=%s*{([^}]+)}') or entry_text:match('doi%s*=%s*"([^"]+)"')
  if doi then
    result.doi = doi
  end
  
  return result
end

-- Function to format authors in APA style
function format_authors(author_string)
  if not author_string then return "" end
  
  -- Remove braces and clean up
  author_string = author_string:gsub("[{}]", "")
  
  -- Split authors on ' and ' (as whole word)
  local authors = {}
  local current_pos = 1
  while current_pos <= #author_string do
    local and_pos = author_string:find(" and ", current_pos)
    if and_pos then
      local author = author_string:sub(current_pos, and_pos - 1)
      author = author:gsub("^%s+", ""):gsub("%s+$", "") -- trim whitespace
      if author ~= "" then
        authors[#authors + 1] = author
      end
      current_pos = and_pos + 5 -- move past " and "
    else
      local author = author_string:sub(current_pos)
      author = author:gsub("^%s+", ""):gsub("%s+$", "") -- trim whitespace
      if author ~= "" then
        authors[#authors + 1] = author
      end
      break
    end
  end
  
  -- Format each author (Last, First Middle -> Last, F. M.)
  local formatted_authors = {}
  for i, author in ipairs(authors) do
    -- Split on comma first to separate last name from first/middle
    local comma_pos = author:find(",")
    if comma_pos then
      local last_name = author:sub(1, comma_pos - 1):gsub("^%s+", ""):gsub("%s+$", "")
      local first_middle = author:sub(comma_pos + 1):gsub("^%s+", ""):gsub("%s+$", "")
      
      local formatted = last_name .. ", "
      
      -- Split first/middle names and convert to initials
      for name in first_middle:gmatch("%S+") do
        if name:len() > 0 then
          formatted = formatted .. name:sub(1,1) .. ". "
        end
      end
      
      formatted_authors[#formatted_authors + 1] = formatted:gsub("%s+$", "") -- trim trailing space
    else
      -- If no comma, assume it's already formatted or just a single name
      formatted_authors[#formatted_authors + 1] = author
    end
  end
  
  -- Join authors with appropriate conjunctions
  if #formatted_authors == 1 then
    return formatted_authors[1]
  elseif #formatted_authors == 2 then
    return formatted_authors[1] .. " & " .. formatted_authors[2]
  else
    local result = ""
    for i = 1, #formatted_authors - 1 do
      result = result .. formatted_authors[i] .. ", "
    end
    result = result .. "& " .. formatted_authors[#formatted_authors]
    return result
  end
end

-- Function to generate full citation from bib entry
function generate_full_citation(bib_entry)
  local parts = {}
  
  -- Authors 
  if bib_entry.author then
    parts[#parts + 1] = format_authors(bib_entry.author)
  end
  
  -- Year
  if bib_entry.year then
    parts[#parts + 1] = '(' .. bib_entry.year .. ')'
  end
  
  -- Journal, volume, pages
  if bib_entry.journal then
    local journal_part = bib_entry.journal
    if bib_entry.volume then
      journal_part = journal_part .. ', ' .. bib_entry.volume
      if bib_entry.number then
        journal_part = journal_part .. '(' .. bib_entry.number .. ')'
      end
    end
    if bib_entry.pages then
      journal_part = journal_part .. ', ' .. bib_entry.pages
    end
    parts[#parts + 1] = journal_part
  end
  
  -- Join parts carefully to avoid double periods
  local result = ""
  if #parts > 0 then
    result = parts[1] -- authors 
    for i = 2, #parts do
      -- Check if the previous part ends with a period
      if result:sub(-1) == "." then
        result = result .. " " .. parts[i]  -- Just add space, no extra period
      else
        result = result .. ". " .. parts[i]  -- Add period and space
      end
    end
  end
  
  -- Add DOI as markdown link if present
  if bib_entry.doi then
    local doi_url = 'https://doi.org/' .. bib_entry.doi
    if result:sub(-1) == "." then
      result = result .. " " .. doi_url  -- Just add space if already ends with period
    else
      result = result .. ". " .. doi_url  -- Add period and space
    end
  else
    if result:sub(-1) ~= "." then
      result = result .. "."  -- Add period only if doesn't already end with one
    end
  end
  
  return result
end

-- Function to load bibliography entries from .bib file
function load_bib_entry(key)
  if bib_cache[key] then
    return bib_cache[key]
  end
  
  -- Try different possible paths for the bibliography file
  local possible_paths = {
    "assets/ref/references.bib",
    "../assets/ref/references.bib",
    "../../assets/ref/references.bib",
    "../../../assets/ref/references.bib"
  }
  
  local content = nil
  for _, bib_file in ipairs(possible_paths) do
    local file = io.open(bib_file, "r")
    if file then
      content = file:read("*all")
      file:close()
      break
    end
  end
  
  if not content then
    -- If we can't find the file, return a fallback
    bib_cache[key] = "[@" .. key .. "]"
    return "[@" .. key .. "]"
  end
  
  -- Find the entry using improved pattern matching
  local start_pos = content:find('@%w+{' .. key .. ',')
  if start_pos then
    -- Find the matching closing brace
    local brace_count = 0
    local pos = start_pos
    local in_entry = false
    local end_pos = nil
    
    while pos <= #content do
      local char = content:sub(pos, pos)
      if char == '{' then
        brace_count = brace_count + 1
        in_entry = true
      elseif char == '}' then
        brace_count = brace_count - 1
        if in_entry and brace_count == 0 then
          end_pos = pos
          break
        end
      end
      pos = pos + 1
    end
    
    if end_pos then
      -- Extract the entry content between the first comma and closing brace
      local entry_start = content:find(',', start_pos) + 1
      local entry_content = content:sub(entry_start, end_pos - 1)
      
      local bib_entry = parse_bib_entry(entry_content, key)
      local full_citation = generate_full_citation(bib_entry)
      bib_cache[key] = full_citation
      return full_citation
    end
  end
  
  -- Fallback if parsing fails
  bib_cache[key] = "[@" .. key .. "]"
  return "[@" .. key .. "]"
end

function Str(elem)
  -- Pattern to match <@key> format  
  local pattern = "<$([%w_]+)>"
  
  if string.match(elem.text, pattern) then
    local results = {}
    local last_pos = 1
    
    for key in string.gmatch(elem.text, pattern) do
      -- Store cited key for bibliography
      cited_keys[key] = true
      
      local full_citation = load_bib_entry(key)
      local start_pos, end_pos = string.find(elem.text, "<$" .. key .. ">", last_pos)
      
      -- Add text before the citation
      if start_pos > last_pos then
        results[#results + 1] = pandoc.Str(string.sub(elem.text, last_pos, start_pos - 1))
      end
      
      -- Parse the citation and create proper elements
      if full_citation and string.match(full_citation, 'https://doi.org/') then
        -- Split citation at DOI URL
        local citation_part, doi_url = full_citation:match('(.+)%. (https://doi.org/.+)$')
        if citation_part and doi_url then
          results[#results + 1] = pandoc.Str(citation_part .. '. ')
          results[#results + 1] = pandoc.Link(doi_url, doi_url)
        else
          results[#results + 1] = pandoc.Str(full_citation)
        end
      else
        results[#results + 1] = pandoc.Str(full_citation or ("[@" .. key .. "]"))
      end
      
      last_pos = end_pos + 1
    end
    
    -- Add remaining text
    if last_pos <= #elem.text then
      results[#results + 1] = pandoc.Str(string.sub(elem.text, last_pos))
    end
    
    return results
  end
  
  return elem
end

-- Add cited references to nocite metadata for automatic bibliography inclusion
function Pandoc(doc)
  -- Process all Str elements first
  doc = doc:walk({Str = Str})
  
  -- If we found any custom citations, add them to the nocite metadata
  if next(cited_keys) then
    -- Get existing nocite metadata or create new one
    local existing_nocite = doc.meta.nocite
    local nocite_list = {}
    
    -- If there's existing nocite, parse it
    if existing_nocite then
      if existing_nocite.t == "MetaInlines" then
        -- Extract existing citations from nocite
        for _, elem in ipairs(existing_nocite) do
          if elem.t == "Str" then
            local text = elem.text
            -- Find citation patterns like @key
            for cite in text:gmatch("@([%w_]+)") do
              nocite_list[#nocite_list + 1] = "@" .. cite
            end
          end
        end
      end
    end
    
    -- Add our custom citation keys to nocite
    for key, _ in pairs(cited_keys) do
      nocite_list[#nocite_list + 1] = "@" .. key
    end
    
    -- Create new nocite metadata
    if #nocite_list > 0 then
      local nocite_string = table.concat(nocite_list, ", ")
      doc.meta.nocite = pandoc.MetaInlines({pandoc.Str(nocite_string)})
    end
  end
  
  return doc
end
