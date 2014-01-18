# Generates a copy of site.pages as site.weighted_pages
# with pages sorted by weight attribute. Pages with no
# weight specified are placed after the pages with specified weight.

module Jekyll

  class FrontPagesGenerator < Generator
    safe true
  
    def generate(site)
      tmp = site.posts.select { |num| num.data['front-page'] }
      site.config['front_pages'] = tmp.sort_by { |a| a.data['front-page'] }
    end
  
  end
end