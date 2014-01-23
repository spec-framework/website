require 'kramdown'
module Jekyll

  class HintBlock < Liquid::Block

    def render(context)
      html = '<div class="hint">
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
      html = '<div class="alert">
	<span class="fa-stack fa-lg alert-icon">
	  <i class="fa fa-square fa-stack-2x"></i>
	  <i class="fa fa-exclamation fa-stack-1x alert-overlay"></i>
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
      html = '<div class="question">
	<span class="fa-stack fa-lg question-icon">
	  <i class="fa fa-circle fa-stack-2x"></i>
	  <i class="fa fa-question fa-stack-1x question-overlay"></i>
	</span>
	<span class="question-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</span>
</div>'
      return html
    end
  end
end

Liquid::Template.register_tag('hint', Jekyll::HintBlock)
Liquid::Template.register_tag('alert', Jekyll::AlertBlock)
Liquid::Template.register_tag('question', Jekyll::QuestionBlock)