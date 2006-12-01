package org.erlide.erlc.builder;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.core.IMakeBuilderInfo;
import org.erlide.erlc.core.IMakeCommonBuildInfo;

public class ErlcMakeNature implements IProjectNature {

	/**
	 * ID of this project nature
	 */
	public static final String NATURE_ID = "org.erlide.erlc.erlcnature";

	private IProject project;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	public void configure() throws CoreException {
		final IMakeBuilderInfo info = ErlideErlcPlugin.createBuildInfo(
				ErlideErlcPlugin.getDefault().getPluginPreferences(),
				ErlcMakeBuilder.BUILDER_ID, false);
		final IMakeBuilderInfo projectInfo = ErlideErlcPlugin.createBuildInfo(
				getProject(), ErlcMakeBuilder.BUILDER_ID);
		projectInfo.setBuildAttribute(IMakeCommonBuildInfo.BUILD_ARGUMENTS,
				info
						.getBuildAttribute(
								IMakeCommonBuildInfo.BUILD_ARGUMENTS, "")); //$NON-NLS-1$
		projectInfo.setBuildAttribute(IMakeCommonBuildInfo.BUILD_COMMAND, info
				.getBuildAttribute(IMakeCommonBuildInfo.BUILD_COMMAND, "make")); //$NON-NLS-1$

		projectInfo.setUseDefaultBuildCmd(info.isDefaultBuildCmd());
		projectInfo.setStopOnError(info.isStopOnError());

		projectInfo.setAutoBuildEnable(info.isAutoBuildEnable());
		projectInfo.setBuildAttribute(IMakeBuilderInfo.BUILD_TARGET_AUTO, info
				.getBuildAttribute(IMakeBuilderInfo.BUILD_TARGET_AUTO, "")); //$NON-NLS-1$

		projectInfo.setIncrementalBuildEnable(info.isIncrementalBuildEnabled());
		projectInfo.setBuildAttribute(
				IMakeBuilderInfo.BUILD_TARGET_INCREMENTAL, info
						.getBuildAttribute(
								IMakeBuilderInfo.BUILD_TARGET_INCREMENTAL,
								"true")); //$NON-NLS-1$

		projectInfo.setFullBuildEnable(info.isIncrementalBuildEnabled());

		projectInfo.setCleanBuildEnable(info.isCleanBuildEnabled());
		projectInfo.setBuildAttribute(IMakeBuilderInfo.BUILD_TARGET_CLEAN, info
				.getBuildAttribute(IMakeBuilderInfo.BUILD_TARGET_CLEAN, "")); //$NON-NLS-1$

		projectInfo.setErrorParsers(info.getErrorParsers());
		projectInfo.setAppendEnvironment(info.appendEnvironment());
		projectInfo.setEnvironment(info.getEnvironment());

		final IProjectDescription desc = project.getDescription();

		final ICommand[] commands = desc.getBuildSpec();

		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(ErlcMakeBuilder.BUILDER_ID)) {
				return;
			}
		}

		final ICommand[] newCommands = new ICommand[commands.length + 1];
		System.arraycopy(commands, 0, newCommands, 0, commands.length);
		final ICommand command = desc.newCommand();
		command.setBuilderName(ErlcMakeBuilder.BUILDER_ID);
		newCommands[newCommands.length - 1] = command;
		desc.setBuildSpec(newCommands);
		project.setDescription(desc, null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	public void deconfigure() throws CoreException {
		final IProjectDescription description = getProject().getDescription();
		final ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(ErlcMakeBuilder.BUILDER_ID)) {
				final ICommand[] newCommands = new ICommand[commands.length - 1];
				System.arraycopy(commands, 0, newCommands, 0, i);
				System.arraycopy(commands, i + 1, newCommands, i,
						commands.length - i - 1);
				description.setBuildSpec(newCommands);
				return;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject() {
		return project;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	public void setProject(IProject project) {
		this.project = project;
	}

	/**
	 * Update the Java command in the build spec (replace existing one if
	 * present, add one first if none).
	 */
	public static IProjectDescription setBuildSpec(
			IProjectDescription description, ICommand newCommand) {

		final ICommand[] oldCommands = description.getBuildSpec();
		final ICommand oldCommand = getBuildSpec(description, newCommand
				.getBuilderName());
		ICommand[] newCommands;

		if (oldCommand == null) {
			// Add a Java build spec before other builders (1FWJK7I)
			newCommands = new ICommand[oldCommands.length + 1];
			System
					.arraycopy(oldCommands, 0, newCommands, 1,
							oldCommands.length);
			newCommands[0] = newCommand;
		} else {
			for (int i = 0, max = oldCommands.length; i < max; i++) {
				if (oldCommands[i].getBuilderName().equals(
						oldCommand.getBuilderName())) {
					oldCommands[i] = newCommand;
					break;
				}
			}
			newCommands = oldCommands;
		}

		// Commit the spec change into the project
		description.setBuildSpec(newCommands);
		return description;
	}

	public static ICommand getBuildSpec(IProjectDescription description,
			String builderID) {
		final ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(builderID)) {
				return commands[i];
			}
		}
		return null;
	}

}
