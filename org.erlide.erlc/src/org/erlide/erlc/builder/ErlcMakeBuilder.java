package org.erlide.erlc.builder;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceRuleFactory;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.core.CommandLauncher;
import org.erlide.erlc.core.ErrorParserManager;
import org.erlide.erlc.core.IMakeBuilderInfo;
import org.erlide.erlc.core.resources.IConsole;
import org.erlide.erlc.internal.core.StreamMonitor;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

public class ErlcMakeBuilder extends ErlangBuilder { // public class
	// ErlcMakeBuilder
	// extends
	// IncrementalProjectBuilder
	// {

	class SampleDeltaVisitor implements IResourceDeltaVisitor {

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
		 */
		public boolean visit(IResourceDelta delta) throws CoreException {
			final IResource resource = delta.getResource();
			switch (delta.getKind()) {
			case IResourceDelta.ADDED:
				// handle added resource
				checkXML(resource);
				break;
			case IResourceDelta.REMOVED:
				// handle removed resource
				break;
			case IResourceDelta.CHANGED:
				// handle changed resource
				checkXML(resource);
				break;
			}
			// return true to continue visiting children.
			return true;
		}
	}

	class SampleResourceVisitor implements IResourceVisitor {

		public boolean visit(IResource resource) {
			checkXML(resource);
			// return true to continue visiting children.
			return true;
		}
	}

	class XMLErrorHandler extends DefaultHandler {

		private IFile file;

		public XMLErrorHandler(IFile file) {
			this.file = file;
		}

		private void addMarker(SAXParseException e, int severity) {
			ErlcMakeBuilder.this.addMarker(file, e.getMessage(), e
					.getLineNumber(), severity);
		}

		@Override
		public void error(SAXParseException exception) throws SAXException {
			addMarker(exception, IMarker.SEVERITY_ERROR);
		}

		@Override
		public void fatalError(SAXParseException exception) throws SAXException {
			addMarker(exception, IMarker.SEVERITY_ERROR);
		}

		@Override
		public void warning(SAXParseException exception) throws SAXException {
			addMarker(exception, IMarker.SEVERITY_WARNING);
		}
	}

	public static final String BUILDER_ID = "org.erlide.erlc.erlcbuilder";

	private static final String MARKER_TYPE = "org.erlide.erlc.xmlProblem";

	private SAXParserFactory parserFactory;

	private void addMarker(IFile file, String message, int lineNumber,
			int severity) {
		try {
			final IMarker marker = file.createMarker(MARKER_TYPE);
			marker.setAttribute(IMarker.MESSAGE, message);
			marker.setAttribute(IMarker.SEVERITY, severity);
			if (lineNumber == -1) {
				lineNumber = 1;
			}
			marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
		} catch (final CoreException e) {
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
//	protected IProject[] xbuild(int kind, Map args, IProgressMonitor monitor)
//			throws CoreException {
//		if (kind == FULL_BUILD) {
//			xfullBuild(monitor);
//		} else {
//			final IResourceDelta delta = getDelta(getProject());
//			if (delta == null) {
//				xfullBuild(monitor);
//			} else {
//				xincrementalBuild(delta, monitor);
//			}
//		}
//		return null;
//	}

	void checkXML(IResource resource) {
		if (resource instanceof IFile && resource.getName().endsWith(".xml")) {
			final IFile file = (IFile) resource;
			deleteMarkers(file);
			final XMLErrorHandler reporter = new XMLErrorHandler(file);
			try {
				getParser().parse(file.getContents(), reporter);
			} catch (final Exception e1) {
			}
		}
	}

	// private void deleteMarkers(IFile file) {
	// try {
	// file.deleteMarkers(MARKER_TYPE, false, IResource.DEPTH_ZERO);
	// } catch (CoreException ce) {
	// }
	// }

	protected void xfullBuild(final IProgressMonitor monitor)
			throws CoreException {
		try {
			getProject().accept(new SampleResourceVisitor());
		} catch (final CoreException e) {
		}
	}

	private SAXParser getParser() throws ParserConfigurationException,
			SAXException {
		if (parserFactory == null) {
			parserFactory = SAXParserFactory.newInstance();
		}
		return parserFactory.newSAXParser();
	}

	protected void xincrementalBuild(IResourceDelta delta,
			IProgressMonitor monitor) throws CoreException {
		// the visitor does the work.
		delta.accept(new SampleDeltaVisitor());
	}

	/**
	 * @see IncrementalProjectBuilder#build
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
			throws CoreException {
		boolean bPerformBuild = true;
		final IMakeBuilderInfo info = ErlideErlcPlugin.createBuildInfo(args,
				BUILDER_ID);
		if (!shouldBuild(kind, info)) {
			return new IProject[0];
		}
		if (kind == IncrementalProjectBuilder.AUTO_BUILD) {
			final IResourceDelta delta = getDelta(getProject());
			if (delta != null) {
				final IResource res = delta.getResource();
				if (res != null) {
					bPerformBuild = res.getProject().equals(getProject());
				}
			} else {
				bPerformBuild = false;
			}
		}
		if (bPerformBuild) {
			final boolean isClean = invokeMake(kind, info, monitor);
			if (isClean) {
				forgetLastBuiltState();
			}
		}
		checkCancel(monitor);
		return getProject().getReferencedProjects();
	}

	/**
	 * Check whether the build has been canceled.
	 */
	public void checkCancel(IProgressMonitor monitor) {
		if (monitor != null && monitor.isCanceled()) {
			throw new OperationCanceledException();
		}
	}

	@Override
	protected void clean(IProgressMonitor monitor) throws CoreException {
		final IMakeBuilderInfo info = ErlideErlcPlugin.createBuildInfo(
				getProject(), BUILDER_ID);
		if (shouldBuild(CLEAN_BUILD, info)) {
			final IResourceRuleFactory ruleFactory = ResourcesPlugin
					.getWorkspace().getRuleFactory();
			final ISchedulingRule rule = ruleFactory.modifyRule(getProject());
			final Job backgroundJob = new Job("Standard Make Builder") { //$NON-NLS-1$

				/*
				 * (non-Javadoc)
				 * 
				 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
				 */
				@Override
				protected IStatus run(IProgressMonitor themonitor) {
					try {
						ResourcesPlugin.getWorkspace().run(
								new IWorkspaceRunnable() {

									public void run(IProgressMonitor amonitor) {
										invokeMake(CLEAN_BUILD, info, amonitor);
									}
								}, rule, IWorkspace.AVOID_UPDATE, themonitor);
					} catch (CoreException e) {
						return e.getStatus();
					}
					IStatus returnStatus = Status.OK_STATUS;
					return returnStatus;
				}

			};

			backgroundJob.setRule(rule);
			backgroundJob.schedule();
		}
	}

	protected String[] getTargets(int kind, IMakeBuilderInfo info) {
		String targets = ""; //$NON-NLS-1$
		switch (kind) {
		case IncrementalProjectBuilder.AUTO_BUILD:
			targets = info.getAutoBuildTarget();
			break;
		case IncrementalProjectBuilder.INCREMENTAL_BUILD: // now treated as
			// the same!
		case IncrementalProjectBuilder.FULL_BUILD:
			targets = info.getIncrementalBuildTarget();
			break;
		case IncrementalProjectBuilder.CLEAN_BUILD:
			targets = info.getCleanBuildTarget();
			break;
		}
		return makeArray(targets);
	}

	protected boolean invokeMake(int kind, IMakeBuilderInfo info,
			IProgressMonitor monitor) {
		boolean isClean = false;
		final IProject currProject = getProject();

		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		monitor.beginTask("Invoking make builder" + currProject.getName(), 100);

		try {
			final IPath buildCommand = info.getBuildCommand();
			if (buildCommand != null) {
				// TODO console, anyone?
				final IConsole console = ErlideErlcPlugin.getDefault()
						.getConsole();
				console.start(currProject);

				final OutputStream cos = console.getOutputStream();

				// remove all markers for this project
				removeAllMarkers(currProject);

				IPath workingDirectory = null;
				if (!info.getBuildLocation().isEmpty()) {
					final IResource res = currProject.getParent().findMember(
							info.getBuildLocation());
					if (res instanceof IContainer && res.exists()) {
						workingDirectory = res.getLocation();
					}
				}
				if (workingDirectory == null) {
					workingDirectory = currProject.getLocation();
				}

				final String[] targets = getTargets(kind, info);
				if (targets.length != 0
						&& targets[targets.length - 1].equals(info
								.getCleanBuildTarget())) {
					isClean = true;
				}

				String errMsg = null;
				final CommandLauncher launcher = new CommandLauncher();
				// Print the command for visual interaction.
				launcher.showCommand(true);

				// Set the environment
				final HashMap<Object, Object> envMap = new HashMap<Object, Object>();
				if (info.appendEnvironment()) {
					envMap.putAll(launcher.getEnvironment());
				}
				// Add variables from build info
				envMap.putAll(info.getExpandedEnvironment());
				final Iterator<Map.Entry<Object, Object>> iter = envMap.entrySet().iterator();
				final List<String> strings = new ArrayList<String>(envMap
						.size());
				while (iter.hasNext()) {
					final Map.Entry<Object, Object> entry = iter.next();
					final StringBuffer buffer = new StringBuffer((String) entry
							.getKey());
					buffer.append('=').append((String) entry.getValue());
					strings.add(buffer.toString());
				}
				final String[] env = strings
						.toArray(new String[strings.size()]);
				String[] buildArguments = targets;
				if (info.isDefaultBuildCmd()) {
					if (!info.isStopOnError()) {
						buildArguments = new String[targets.length + 1];
						buildArguments[0] = "-k"; //$NON-NLS-1$
						System.arraycopy(targets, 0, buildArguments, 1,
								targets.length);
					}
				} else {
					final String args = info.getBuildArguments();
					if (args != null && !args.equals("")) { //$NON-NLS-1$
						final String[] newArgs = makeArray(args);
						buildArguments = new String[targets.length
								+ newArgs.length];
						System.arraycopy(newArgs, 0, buildArguments, 0,
								newArgs.length);
						System.arraycopy(targets, 0, buildArguments,
								newArgs.length, targets.length);
					}
				}
				// MakeRecon recon = new MakeRecon(buildCommand, buildArguments,
				// env,
				// workingDirectory, makeMonitor, cos);
				// recon.invokeMakeRecon();
				// cos = recon;
				final QualifiedName qName = new QualifiedName(ErlideErlcPlugin
						.getUniqueIdentifier(), "progressMonitor"); //$NON-NLS-1$
				Integer last = (Integer) getProject().getSessionProperty(qName);
				if (last == null) {
					last = new Integer(100);
				}
				final StreamMonitor streamMon = new StreamMonitor(
						new SubProgressMonitor(monitor, 100), cos, last
								.intValue());
				final ErrorParserManager epm = new ErrorParserManager(
						getProject(), workingDirectory, this, info
								.getErrorParsers());
				epm.setOutputStream(streamMon);
				final OutputStream stdout = epm.getOutputStream();
				final OutputStream stderr = epm.getOutputStream();
				// Sniff console output for scanner info
				// ConsoleOutputSniffer sniffer =
				// ScannerInfoConsoleParserFactory.getMakeBuilderOutputSniffer(
				// stdout, stderr, getProject(), workingDirectory, null, this,
				// null);
				// OutputStream consoleOut = (sniffer == null ? stdout :
				// sniffer.getOutputStream());
				// OutputStream consoleErr = (sniffer == null ? stderr :
				// sniffer.getErrorStream());

				final OutputStream consoleOut = stdout;
				final OutputStream consoleErr = stderr;

				final Process p = launcher.execute(buildCommand,
						buildArguments, env, workingDirectory);
				if (p != null) {
					try {
						// Close the input of the Process explicitly.
						// We will never write to it.
						p.getOutputStream().close();
					} catch (final IOException e) {
					}
					// Before launching give visual cues via the monitor
					monitor.subTask("Invoking command "
							+ launcher.getCommandLine());
					if (launcher.waitAndRead(consoleOut, consoleErr,
							new SubProgressMonitor(monitor, 0)) != CommandLauncher.OK) {
						errMsg = launcher.getErrorMessage();
					}
					monitor.subTask("Updating project");

					try {
						// Do not allow the cancel of the refresh, since the
						// builder is
						// external
						// to Eclipse, files may have been created/modified and
						// we will be
						// out-of-sync.
						// The caveat is for hugue projects, it may take
						// sometimes at
						// every build.
						currProject
								.refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (final CoreException e) {
					}
				} else {
					errMsg = launcher.getErrorMessage();
				}
				getProject().setSessionProperty(
						qName,
						!monitor.isCanceled() && !isClean ? new Integer(
								streamMon.getWorkDone()) : null);

				if (errMsg != null) {
					StringBuffer buf = new StringBuffer(buildCommand.toString()
							+ " "); //$NON-NLS-1$
					for (String element : buildArguments) {
						buf.append(element);
						buf.append(' ');
					}

					final String errorDesc = "Error launching builder ("
							+ buf.toString() + ")";
					buf = new StringBuffer(errorDesc);
					buf.append(System.getProperty("line.separator", "\n")); //$NON-NLS-1$ //$NON-NLS-2$
					buf.append("(").append(errMsg).append(")"); //$NON-NLS-1$ //$NON-NLS-2$
					cos.write(buf.toString().getBytes());
					cos.flush();
				}

				stdout.close();
				stderr.close();

				monitor.subTask("Creating markers");
				consoleOut.close();
				consoleErr.close();
				epm.reportProblems();
				cos.close();
			}
		} catch (final Exception e) {
			ErlangPlugin.log(e);
		} finally {
			monitor.done();
		}
		return (isClean);
	}

	// Turn the string into an array.
	String[] makeArray(String string) {
		string = string.trim();
		final char[] array = string.toCharArray();
		final ArrayList<String> aList = new ArrayList<String>();
		StringBuffer buffer = new StringBuffer();
		boolean inComment = false;
		for (int i = 0; i < array.length; i++) {
			final char c = array[i];
			if (array[i] == '"' || array[i] == '\'') {
				if (i > 0 && array[i - 1] == '\\') {
					inComment = false;
				} else {
					inComment = !inComment;
				}
			}
			if (c == ' ' && !inComment) {
				aList.add(buffer.toString());
				buffer = new StringBuffer();
			} else {
				buffer.append(c);
			}
		}
		if (buffer.length() > 0) {
			aList.add(buffer.toString());
		}
		return aList.toArray(new String[aList.size()]);
	}

	private void removeAllMarkers(IProject currProject) throws CoreException {
		final IWorkspace workspace = currProject.getWorkspace();

		// remove all markers
		final IMarker[] markers = currProject.findMarkers(
				ErlangBuilder.PROBLEM_MARKER, true, IResource.DEPTH_INFINITE);
		if (markers != null) {
			workspace.deleteMarkers(markers);
		}
	}

	protected boolean shouldBuild(int kind, IMakeBuilderInfo info) {
		switch (kind) {
		case IncrementalProjectBuilder.AUTO_BUILD:
			return info.isAutoBuildEnable();
		case IncrementalProjectBuilder.INCREMENTAL_BUILD: // now treated as
			// the same!
		case IncrementalProjectBuilder.FULL_BUILD:
			return info.isFullBuildEnabled() | info.isIncrementalBuildEnabled();
		case IncrementalProjectBuilder.CLEAN_BUILD:
			return info.isCleanBuildEnabled();
		}
		return true;
	}
}
