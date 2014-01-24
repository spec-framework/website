require 'kramdown'
module Jekyll

  class HintBlock < Liquid::Block

    def render(context)
      html = '<div class="alert hint">
	<span class="fa-stack fa-lg hint-icon">
	  <i class="fa fa-square fa-stack-2x"></i>
	  <i class="fa fa-info fa-stack-1x hint-overlay"></i>
	</span>
	<span class="hint-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</span>
</div>'
      return html
    end
  end

  class AlertBlock < Liquid::Block

    def render(context)
      html = '<div class="alert alertbox">
	<span class="fa-stack fa-lg alert-icon">
	  <i class="fa fa-exclamation-triangle fa-2x"></i>
	</span>
	<span class="alert-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</span>
</div>'
      return html
    end
  end
  
  class QuestionBlock < Liquid::Block

    def render(context)
      html = '<div class="alert question">
	<span class="fa-stack fa-lg question-icon">
	  <i class="fa fa-question-circle fa-2x"></i>
	</span>
	<span class="question-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</span>
</div>'
      return html
    end
  end
end

Liquid::Template.register_tag('info', Jekyll::HintBlock)
Liquid::Template.register_tag('alert', Jekyll::AlertBlock)
Liquid::Template.register_tag('question', Jekyll::QuestionBlock)