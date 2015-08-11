package com.sungardas.snapdirector.worker;

import java.io.OutputStream;
import java.io.PrintWriter;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class CommandLineArgumentsProvider {
	private static final String WORKER_ID_ARG = "worker-id";
	private CommandLine commandLine;
	private String workerInstanceId;

	public CommandLineArgumentsProvider(String[] args) {
		commandLine = parseArguments(getOptions(), args);
		workerInstanceId = commandLine.getOptionValue(WORKER_ID_ARG);
	}
	
	public boolean hasWorkerInstanceId() {
		return workerInstanceId!=null;
	}
	public String getWorkerInstanceId() {
		return workerInstanceId;
	}

	private static Options getOptions() {
		Option workerId = new Option("w", WORKER_ID_ARG, false, "Worker's configuration identifier");
		workerId.setArgs(1);
		workerId.setArgName(WORKER_ID_ARG);
		workerId.setRequired(true);
		Options opts = new Options();
		opts.addOption(workerId);
		return opts;
	}

	private CommandLine parseArguments(Options options, String[] commandLineArguments) {
		CommandLine commandLine = null;
		try {
			CommandLineParser cmdLinePosixParser = new DefaultParser();
			commandLine = cmdLinePosixParser.parse(options, commandLineArguments);
		} catch (ParseException e) {
			printHelp(options, 120, "Options:", "", 3, 5, true, System.out);
			System.exit(-1);
		}
		return commandLine;
	}

	private void printHelp(final Options options, final int printedRowWidth, final String header, final String footer,
			final int spacesBeforeOption, final int spacesBeforeOptionDescription, final boolean displayUsage,
			final OutputStream out) {
		final String commandLineSyntax = "java -jar snapdirector.jar";
		final PrintWriter writer = new PrintWriter(out);
		final HelpFormatter helpFormatter = new HelpFormatter();
		helpFormatter.printHelp(writer, printedRowWidth, commandLineSyntax, header, options, spacesBeforeOption,
				spacesBeforeOptionDescription, footer, displayUsage);
		writer.flush();
	}

}
