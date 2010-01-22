package org.erlide.wrangler.refactoring.selection.internal;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.wrangler.refactoring.selection.IErlSelection;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

public abstract class AbstractErlSelection implements IErlSelection {

	protected IFile file;

	public boolean isEmpty() {
		return false;
	}

	public OtpErlangList getSearchPath() {
		IProject project = file.getProject();
		IErlModel model = ErlangCore.getModel();
		IErlProject actualProject = model.getErlangProject(project.getName());
		OldErlangProjectProperties prop = actualProject.getProperties();
		IPath projectLocation = actualProject.getProject().getLocation();

		List<String> sourcDirs = prop.getSourceDirs();
		OtpErlangString[] searchPath = new OtpErlangString[sourcDirs.size()];
		for (int i = 0; i < sourcDirs.size(); ++i) {
			searchPath[i] = new OtpErlangString(projectLocation.append(
					sourcDirs.get(i)).toOSString());
		}
		return new OtpErlangList(searchPath);
	}

	public String getFilePath() {
		return file.getLocation().toOSString();
	}

	public IFile getFile() {
		return file;
	}

}
