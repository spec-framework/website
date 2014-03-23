module Jekyll
  module AssetFilter
    def toJQuery(input)
      iChars = "~`!#$\%^&*+=-[]\\\';,/{}|\":<>?";
      result = ""
      input.each_char{ | c | 
        if( c != ' ')
           if(!iChars.include?(c))
             result += c
           end
         end
      }
      result
    end
  end
end

Liquid::Template.register_filter(Jekyll::AssetFilter)
