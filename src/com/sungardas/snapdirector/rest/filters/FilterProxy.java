package com.sungardas.snapdirector.rest.filters;


import javax.servlet.*;
import java.io.IOException;

import org.apache.tomcat.util.http.fileupload.ThresholdingOutputStream;


public class FilterProxy implements Filter {

	private Filter filter;

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {

	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		if (filter == null) {
			chain.doFilter(request, response);
		} else {
			filter.doFilter(request, response, chain);
		}
	}

	@Override
	public void destroy() {

	}

	public Filter getFilter() {
		return filter;
	}

	public void setFilter(Filter filter) {
		this.filter = filter;
	}
}
