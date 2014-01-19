require 'kramdown'
module Jekyll
  class PillarConverter < Converter
    safe true
    priority :low

    def matches(ext)
      ext =~ /^\.pier$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      md = `cd _pillar ; echo "#{content}" | ./pharo Pharo.image export --to=markdown --configurationFile="conf.conf"`
      doc = Kramdown::Document.new(md, :input => 'markdown')
      doc.to_html
    end
  end
end