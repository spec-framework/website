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
      md = `echo "#{content}" | _pillar/pharo _pillar/Pharo.image export --to=markdown --configurationFile="_pillar/conf.conf"`
      doc = Kramdown::Document.new(md, :input => 'markdown')
      doc.to_html
    end
  end
end