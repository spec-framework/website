require 'kramdown'
module Jekyll

  class HintBlock < Liquid::Block

    def render(context)
      html = '<div class="hint_box hint">
      <div class="hint_box_row">
	<div class="hint_box_icon fa-stack fa-lg hint-icon">
	  <i class="fa fa-square fa-stack-2x"></i>
	  <i class="fa fa-info fa-stack-1x hint-overlay"></i>
	</div>
	<div class="hint_box_text hint-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</div>
</div></div>'
      return html
    end
  end

  class AlertBlock < Liquid::Block

    def render(context)
      html = '<div class="hint_box alertbox">
      <div class="hint_box_row">
	<div class="hint_box_icon fa-stack fa-lg alert-icon">
	  <i class="fa fa-exclamation-triangle fa-2x"></i>
	</div>
	<div class="hint_box_text alert-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</div>
</div></div>'
      return html
    end
  end
  
  class QuestionBlock < Liquid::Block

    def render(context)
      html = '<div class="hint_box question">
      <div class="hint_box_row">
	<div class="hint_box_icon fa-stack fa-lg question-icon">
	  <i class="fa fa-question-circle fa-2x"></i>
	</div>
	<div class="hint_box_text question-text">'
      html += Kramdown::Document.new(super.strip, :input => 'markdown').to_html
      html += '</div>
</div></div>'
      return html
    end
  end
end

Liquid::Template.register_tag('info', Jekyll::HintBlock)
Liquid::Template.register_tag('alert', Jekyll::AlertBlock)
Liquid::Template.register_tag('question', Jekyll::QuestionBlock)