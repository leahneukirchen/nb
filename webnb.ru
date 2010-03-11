#!/usr/bin/env rackup
# -*- ruby -*-

run lambda { |env|
  req = Rack::Request.new(env)
  res = Rack::Response.new

  arg = req["q"] || req.path.split('/')[1] || ""
  
  begin
    txt = File.read(File.expand_path("~/NB/#{arg.delete("^a-zA-Z0-9.-")}"))
  rescue Errno::ENOENT, Errno::EISDIR
    arg.delete!("'\n")
    txt = `nb '#{arg}'`
    if txt.split("\n").size == 1
      res.redirect(req.script_name + "/" + txt[/^[\w-]+/])
      return res.finish
    end
    txt.gsub!(/^[\w-]+/, '[[\&]]')    
  end

  txt = Rack::Utils.escape_html(txt).
                    gsub(/\[\[(.*?)\]\]/, %q{<a href="\1">\1</a>})

  res.write <<EOF
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>webnb: #{arg}</title>
  </head>
  <body>
    <form><input name="q"><input type="submit" value="Search"></form>
    <pre>#{txt}</pre>
  </body>
</html>
EOF

  res.finish
}
