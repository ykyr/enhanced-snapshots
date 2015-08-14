package com.sungardas.snapdirector.worker;


import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebInitParam;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Servlet implementation class TaskWorkerInitializerServlet
 */
@WebServlet(
		loadOnStartup=3,
		urlPatterns = { "/TaskSenderInitializerServlet" }, 
		initParams = { 
				@WebInitParam(name = "shutdown-on-unload", value = "true")
		})
public class TaskSenderInitializerServlet extends HttpServlet {
	private static final Log LOG = LogFactory.getLog(TaskSenderInitializerServlet.class);
	private static final long serialVersionUID = 1L;
	private ExecutorService  executor;
       
    public TaskSenderInitializerServlet() {
        super();
    }

	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		LOG.info("TaskSenderInitializerServlet started...");
		executor = Executors.newSingleThreadExecutor();
		executor.execute(new TasksSender(config.getServletContext()));
	}

	public void destroy() {
		LOG.info("TaskSenderInitializerServlet stoped.");
		executor.shutdownNow();
	}
	
	
	
}
