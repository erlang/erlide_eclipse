package org.erlide.core.erlang.internal;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModelStatusConstants;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlideUtil;
import org.erlide.core.util.PluginUtils;

/**
 * Implementation of folder in erlang model
 * 
 * @author Jakob C
 * 
 */
public class ErlFolder extends Openable implements IErlFolder {
	private final IFolder folder;

	protected ErlFolder(final IFolder folder, final IErlElement parent) {
		super(parent, folder.getName());
		this.folder = folder;
	}

	@Override
	protected boolean buildStructure(final IProgressMonitor pm,
			final IResource underlyingResource) throws ErlModelException {
		logBuildStructure(underlyingResource);
		final IErlModelManager manager = ErlangCore.getModelManager();
		final IContainer c = (IContainer) underlyingResource;
		try {
			// FIXME this is general stuff, should we put it in, say, model or
			// model manager?
			final IResource[] members = c.members();
			for (final IResource resource : members) {
				manager.create(resource, this);
			}
		} catch (final CoreException e) {
			throw new ErlModelException(e,
					IErlModelStatusConstants.CORE_EXCEPTION);
		}
		return true;
	}

	@Override
	protected void closing(final Object info) throws ErlModelException {
		// FIXME general stuff, move to model or manager
		for (final IErlElement e : getChildren()) {
			if (e instanceof ErlElement) {
				final ErlElement ee = (ErlElement) e;
				ee.closing(info);
			}
		}
	}

	public static IErlModule getModule(final IParent parent, final String name) {
		try {
			for (final IErlElement e : parent.getChildren()) {
				if (e instanceof IErlModule) {
					final IErlModule m = (IErlModule) e;
					if (m.getName().equals(name)) {
						return m;
					}
				} else if (e instanceof IParent) {
					final IParent p = (IParent) e;
					final IErlModule m = getModule(p, name);
					if (m != null) {
						return m;
					}
				}
			}
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
		return null;
	}

	public IErlModule getModule(final String name) throws ErlModelException {
		return getModule(this, name);
	}

	public List<IErlModule> getModules() throws ErlModelException {
		return getModules(this);
	}

	public static List<IErlModule> getModules(final IParent parent)
			throws ErlModelException {
		final List<IErlModule> result = new LinkedList<IErlModule>();
		for (final IErlElement e : parent.getChildren()) {
			if (e instanceof IErlFolder) {
				final IErlFolder f = (IErlFolder) e;
				if (f.isOnSourcePath()) { // FIXME is this what you want?
					result.addAll(f.getModules());
				}
			} else if (e instanceof IErlModule) {
				result.add((IErlModule) e);
			}
		}
		return result;
	}

	public IResource[] getNonErlangResources() throws ErlModelException {
		// TODO Auto-generated method stub
		return null;
	}

	public Kind getKind() {
		return Kind.FOLDER;
	}

	public IResource getResource() {
		return folder;
	}

	public boolean isOnSourcePath() {
		return PluginUtils.isOnSourcePath(folder);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.core.erlang.internal.ErlElement#getChildNamed(java.lang.String)
	 */
	@Override
	public IErlElement getChildNamed(final String name) {
		return super.getChildNamed(ErlideUtil.withoutExtension(name));
	}
}
