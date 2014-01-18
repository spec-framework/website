require 'rubygems'
require 'json'
    

module Jekyll

  class Data

    @data = []
    
    def data
      return @data
    end
    def setData (newData)
      @data = newData
    end
  end

  class TestPlugin < Liquid::Tag
    
    def initialize(tag_name, text, tokens)
      super
      @text = text.strip
    end
  
    def getContributors
      raw = `curl -s "https://api.github.com/repos/#{@text}/contributors"`
      parsed = JSON.parse(raw)
      logins = parsed.collect { | each | each["login"] }
      datas = logins.collect { | each | 
        data = Data.new
        data.setData(JSON.parse(`curl -s "https://api.github.com/users/#{each}"`))
        data
      }
    end
    
    def joinAsSentence
      data = getContributors.collect { | each | 
        "<a href=\"https://github.com/#{each.data["login"]}\">"+each.data["name"]+"</a>" }
      
      result = ""
      data.each_index { |index| 
        each = data[index]
        if index != 0
          if index == (data.size() -1)
            result += ' and '
          else
            result += ', '
          end
        end
      
        result += each
      }  
      result
    end
  
    def render(context)
      joinAsSentence
    end
  
  end
end

Liquid::Template.register_tag('contributors', Jekyll::TestPlugin)