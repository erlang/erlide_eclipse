/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlModelStatus;
import org.erlide.core.erlang.ErlModelStatusConstants;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementVisitor;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.ErlLogger;
import org.erlide.runtime.backend.Backend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideBackend;

/**
 * Root of Erlang element handle hierarchy.
 * 
 * @see IErlElement
 */
public abstract class ErlElement extends PlatformObject implements IErlElement,
		Cloneable {

	public static final char EM_ESCAPE = '\\';

	public static final char EM_PROJECT = '=';

	public static final char EM_PACKAGE = '/';

	public static final char EM_MODULE = '{';

	public static final char EM_ATTRIBUTE = '<';

	public static final char EM_FUNCTION = '^';

	public static final char EM_LIBRARY = '~';

	public static final char EM_BEAMFILE = '|';

	public static final char EM_COUNT = '!';

	/**
	 * A count to uniquely identify this element in the case that a duplicate
	 * named element exists. For example, if there are two fields in a
	 * compilation unit with the same name, the occurrence count is used to
	 * distinguish them. The occurrence count starts at 1 (thus the first
	 * occurrence is occurrence 1, not occurrence 0).
	 */
	public int fOccurrenceCount = 1;

	/**
	 * This element's parent, or <code>null</code> if this element does not have
	 * a parent.
	 */
	protected IErlElement fParent;

	/**
	 * This element's name, or an empty <code>String</code> if this element does
	 * not have a name.
	 */
	protected String fName;

	public static final ErlElement[] NO_ELEMENTS = new ErlElement[0];

	protected static final Object NO_INFO = new Object();

	/**
	 * Constructs a handle for a Erlang element with the given parent element
	 * and name.
	 * 
	 * @param parent
	 *            The parent of Erlang element
	 * @param name
	 *            The name of Erlang element
	 * 
	 * @throws IllegalArgumentException
	 *             if the type is not one of the valid Erlang element type
	 *             constants
	 * 
	 */
	protected ErlElement(final IErlElement parent, final String name) {
		fParent = parent;
		fName = name;
		Assert.isNotNull(fName);
	}

	/**
	 * @see IOpenable
	 */
	public void close() throws ErlModelException {
		// /ErlModelManager.getErlangModelManager().removeInfoAndChildren(this);
	}

	/**
	 * This element is being closed. Do any necessary cleanup.
	 */
	protected abstract void closing(Object info) throws ErlModelException;

	/**
	 * Returns true if this handle represents the same Erlang element as the
	 * given handle. By default, two handles represent the same element if they
	 * are identical or if they represent the same type of element, have equal
	 * names, parents, and occurrence counts.
	 * 
	 * <p>
	 * If a subclass has other requirements for equality, this method must be
	 * overridden.
	 * 
	 * @see Object#equals
	 */
	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null) {
			return false;
		}

		// Erlang model parent is null
		if (fParent == null) {
			return super.equals(o);
		}

		if (o instanceof ErlElement) { // WHY OH WHY?!?!?!? This was a tough
			// bug (jc)
			// assume instanceof check is done in subclass
			final ErlElement other = (ErlElement) o;
			return fOccurrenceCount == other.fOccurrenceCount
					&& fName.equals(other.fName)
					&& fParent.equals(other.fParent);
		}
		return false;
	}

	protected void escapeMementoName(final StringBuilder buffer,
			final String mementoName) {
		for (int i = 0, length = mementoName.length(); i < length; i++) {
			final char character = mementoName.charAt(i);
			switch (character) {
			case EM_ESCAPE:
			case EM_COUNT:
			case EM_PROJECT:
			case EM_PACKAGE:
			case EM_MODULE:
			case EM_ATTRIBUTE:
			case EM_FUNCTION:
			case EM_LIBRARY:
			case EM_BEAMFILE:
				buffer.append(EM_ESCAPE);
			}
			buffer.append(character);
		}
	}

	/**
	 * @see IErlElement
	 */
	public boolean exists() {
		return true;
	}

	/**
	 * @see IErlElement
	 */
	public IErlElement getAncestor(final Kind ancestorType) {

		IErlElement element = this;
		while (element != null) {
			if (element.getKind() == ancestorType) {
				return element;
			}
			element = element.getParent();
		}
		return null;
	}

	/**
	 * Returns a collection of (immediate) children of this node of the
	 * specified type.
	 * 
	 * @param type
	 *            - one of the constants defined by IErlElement
	 */
	public ArrayList<? extends IErlElement> getChildrenOfType(final Kind type)
			throws ErlModelException {
		final ArrayList<IErlElement> list = new ArrayList<IErlElement>();
		for (final IErlElement i : getChildren()) {
			if (i.getKind() == type) {
				list.add(i);
			}
		}
		return list;
	}

	/**
	 * @see IMember
	 */
	public IErlModule getModule() {
		return null;
	}

	/**
	 * @see IErlElement
	 */
	public String getName() {
		return fName;
	}

	/**
	 * @see IErlElement
	 */
	public ErlModel getModel() {
		IErlElement current = this;
		do {
			if (current instanceof ErlModel) {
				return (ErlModel) current;
			}
		} while ((current = current.getParent()) != null);
		return null;
	}

	/**
	 * @see IErlElement
	 */
	public IErlProject getErlProject() {
		IErlElement current = this;
		do {
			if (current instanceof IErlProject) {
				return (IErlProject) current;
			}
		} while ((current = current.getParent()) != null);
		return null;
	}

	/**
	 * Return the first instance of IOpenable in the parent hierarchy of this
	 * element.
	 * 
	 * <p>
	 * Subclasses that are not IOpenable's must override this method.
	 */
	public IOpenable getOpenableParent() {
		return (IOpenable) fParent;
	}

	/**
	 * @see IErlElement
	 */
	public IErlElement getParent() {
		return fParent;
	}

	/**
	 * Returns the element that is located at the given source position in this
	 * element. This is a helper method for <code>IErlModule#getElementAt</code>
	 * , and only works on compilation units and types. The position given is
	 * known to be within this element's source range already, and if no finer
	 * grained element is found at the position, this element is returned.
	 */
	protected IErlElement getSourceElementAt(final int position)
			throws ErlModelException {
		if (this instanceof ISourceReference) {
			for (final IErlElement i : fChildren) {
				if (i instanceof SourceRefElement) {
					final SourceRefElement child = (SourceRefElement) i;
					final ISourceRange range = child.getSourceRange();
					final int start = range.getOffset();
					final int end = start + range.getLength();
					if (start <= position && position <= end) {
						if (child instanceof IParent) {
							return child.getSourceElementAt(position);
						}
						return child;
					}
				}
			}
		} else {
			// should not happen
			Assert.isTrue(false);
		}
		return this;
	}

	/*
	 * (non-Edoc)
	 * 
	 * @see org.erlide.core.erlang.IErlElement#getSchedulingRule()
	 */
	public ISchedulingRule getSchedulingRule() {
		final IResource resource = getResource();
		if (resource == null) {
			class NoResourceSchedulingRule implements ISchedulingRule {

				public IPath fPath;

				public NoResourceSchedulingRule(final IPath path) {
					fPath = path;
				}

				public boolean contains(final ISchedulingRule rule) {
					if (rule instanceof NoResourceSchedulingRule) {
						return fPath
								.isPrefixOf(((NoResourceSchedulingRule) rule).fPath);
					}
					return false;
				}

				public boolean isConflicting(final ISchedulingRule rule) {
					if (rule instanceof NoResourceSchedulingRule) {
						final IPath otherPath = ((NoResourceSchedulingRule) rule).fPath;
						return fPath.isPrefixOf(otherPath)
								|| otherPath.isPrefixOf(fPath);
					}
					return false;
				}
			}
			return new NoResourceSchedulingRule(getResource()
					.getProjectRelativePath());
		}
		return resource;
	}

	/**
	 * @see IParent
	 */
	public boolean hasChildren() {
		// if I am not open, return true to avoid opening (case of a Erlang
		// project, a compilation unit or a class file).
		// also see https://bugs.eclipse.org/bugs/show_bug.cgi?id=52474
		final Object elementInfo = ErlangCore.getModelManager().getInfo(this);
		if (elementInfo instanceof ErlElement) {
			return !fChildren.isEmpty();
		}
		return true;
	}

	/**
	 * Returns the hash code for this Erlang element. By default, the hash code
	 * for an element is a combination of its name and parent's hash code.
	 * Elements with other requirements must override this method.
	 */
	@Override
	public int hashCode() {
		if (fParent == null) {
			return super.hashCode();
		}
		return Util.combineHashCodes(fName.hashCode(), fParent.hashCode());
	}

	/**
	 * Returns true if this element is an ancestor of the given element,
	 * otherwise false.
	 */
	public boolean isAncestorOf(final IErlElement e) {
		IErlElement parentElement = e.getParent();
		while (parentElement != null && !parentElement.equals(this)) {
			parentElement = parentElement.getParent();
		}
		return parentElement != null;
	}

	/**
	 * @see IErlElement
	 */
	public boolean isReadOnly() {
		return false;
	}

	/**
	 * Creates and returns and not present exception for this element.
	 */
	protected ErlModelException newNotPresentException() {
		ErlLogger.debug("not found: " + fName);
		return new ErlModelException(new ErlModelStatus(
				ErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST, this));
	}

	/**
	 */
	public String readableName() {
		return getName();
	}

	protected String tabString(final int tab) {
		return " ";
		// final StringBuilder buffer = new StringBuilder();
		// for (int i = tab; i > 0; i--) {
		// buffer.append(" "); //$NON-NLS-1$
		// }
		// return buffer.toString();
	}

	/**
	 * Debugging purposes
	 */
	public String toDebugString() {
		final StringBuilder buffer = new StringBuilder();
		this.toStringInfo(0, buffer, NO_INFO);
		return buffer.toString();
	}

	/**
	 * Debugging purposes
	 */
	@Override
	public String toString() {
		final StringBuilder buffer = new StringBuilder();
		toString(0, buffer);
		return buffer.toString();
	}

	/**
	 * Debugging purposes
	 */
	protected void toString(final int tab, final StringBuilder buffer) {
		final Object info = this.toStringInfo(tab, buffer);
		if (tab == 0) {
			toStringAncestors(buffer);
		}
		toStringChildren(tab, buffer, info);
	}

	/**
	 * Debugging purposes
	 */
	public String toStringWithAncestors() {
		final StringBuilder buffer = new StringBuilder();
		this.toStringInfo(0, buffer, NO_INFO);
		toStringAncestors(buffer);
		return buffer.toString();
	}

	/**
	 * Debugging purposes
	 */
	protected void toStringAncestors(final StringBuilder buffer) {
		final ErlElement parentElement = (ErlElement) getParent();
		if (parentElement != null && parentElement.getParent() != null) {
			buffer.append("[> "); //$NON-NLS-1$
			parentElement.toStringInfo(0, buffer, NO_INFO);
			parentElement.toStringAncestors(buffer);
			buffer.append("] "); //$NON-NLS-1$
		}
	}

	/**
	 * Debugging purposes
	 */
	protected void toStringChildren(final int tab, final StringBuilder buffer,
			final Object info) {
		if (info == null || !(info instanceof ErlElement)) {
			return;
		}
		if (fChildren.size() > 0) {
			buffer.append("{");
		}
		for (final IErlElement element : fChildren) {
			((ErlElement) element).toString(tab + 1, buffer);
			buffer.append(","); //$NON-NLS-1$
		}
		if (fChildren.size() > 0) {
			buffer.deleteCharAt(buffer.length() - 1);
			buffer.append("}");
		}
	}

	/**
	 * Debugging purposes
	 */
	public Object toStringInfo(final int tab, final StringBuilder buffer) {
		final Object info = ErlangCore.getModelManager().getInfo(this);
		this.toStringInfo(tab, buffer, info);
		return info;
	}

	/**
	 * Debugging purposes
	 */
	protected void toStringInfo(final int tab, final StringBuilder buffer,
			final Object info) {
		buffer.append(tabString(tab));
		toStringName(buffer);
		if (info == null) {
			buffer.append(" (not open)"); //$NON-NLS-1$
		}
	}

	/**
	 * Debugging purposes
	 */
	protected void toStringName(final StringBuilder buffer) {
		buffer.append(getName());
		if (fOccurrenceCount > 1) {
			buffer.append("#"); //$NON-NLS-1$
			buffer.append(fOccurrenceCount);
		}
	}

	/**
	 * Collection of handles of immediate children of this object. This is an
	 * empty array if this element has no children.
	 */
	protected List<IErlElement> fChildren = new ArrayList<IErlElement>();

	/**
	 * Is the structure of this element known
	 * 
	 * @see IErlElement#isStructureKnown()
	 */
	protected boolean structureKnown = false;

	/**
	 * Shared empty collection used for efficiency.
	 */
	static final IProject[] NO_NON_ERLANG_RESOURCES = new IProject[] {};

	public void addChild(final IErlElement child) {
		fChildren.add(child);
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (final CloneNotSupportedException e) {
			throw new Error();
		}
	}

	public List<? extends IErlElement> getChildren() {
		return fChildren;
	}

	public IErlElement getChildNamed(final String name) {
		if (this instanceof IParent) {
			return getChildNamed((IParent) this, name);
		}
		return null;
	}

	/**
	 * Returns <code>true</code> if this child is in my children collection
	 */
	protected boolean includesChild(final IErlElement child) {
		return fChildren.contains(child);
	}

	/**
	 * @see IErlElement#isStructureKnown()
	 */
	public boolean isStructureKnown() {
		return structureKnown;
	}

	public void removeChild(final IErlElement child) {
		fChildren.remove(child);
	}

	public void setChildren(final Collection<? extends IErlElement> c) {
		fChildren.clear();
		fChildren.addAll(c);
	}

	public void setChildren(final IErlElement[] children) {
		fChildren.clear();
		for (final IErlElement i : children) {
			fChildren.add(i);
		}
	}

	/**
	 * Sets whether the structure of this element known
	 * 
	 * @see IErlElement#isStructureKnown()
	 */
	public void setStructureKnown(final boolean newStructureKnown) {
		structureKnown = newStructureKnown;
	}

	public void resourceChanged() {
		// FIXME is this enough? it will rebuild at next occasion, and modules
		// are handled with reconciles, containers children through add and
		// remove, but... e.g. name change of folder?
		setStructureKnown(false);
	}

	protected String pp(final OtpErlangObject e) {
		if (e == null) {
			return "";
		}
		if (e instanceof OtpErlangList) {
			// final OtpErlangList ll = (OtpErlangList) e;
			// String r = "";
			// for (int i = 0; i < ll.arity(); i++) {
			// final OtpErlangObject x = ll.elementAt(i);
			// r = r + pp(x) + ", ";
			// }
			// final String rr = r.length() > 2 ? r.substring(0, r.length() - 2)
			// : "";
			final String rr = pp_1((OtpErlangList) e);
			return "(" + rr + ")";
		} else if (e instanceof OtpErlangTuple) {
			try {
				final Backend b = ErlangCore.getBackendManager()
						.getIdeBackend();
				return ErlideBackend.prettyPrint(b, e);
			} catch (final Exception e1) {
				return "?";
			}
		} else {
			return e.toString();
		}
	}

	protected String pp_1(final OtpErlangList e) {
		if (e == null) {
			return "";
		}
		String r = "";
		for (int i = 0; i < e.arity(); i++) {
			// final OtpErlangList x = (OtpErlangList) e.elementAt(i);
			final OtpErlangTuple x = (OtpErlangTuple) e.elementAt(i);
			r = r + pp_2(x);
		}
		return r;
		// final String rr = r.length() > 2 ? r.substring(0, r.length() - 2) :
		// "";
		// return rr;
	}

	protected String pp_2(final OtpErlangTuple x2) {
		// if (x2 == null) {
		// return "";
		// }
		// String r = "";
		// for (int i = 0; i < x2.arity(); i++) {
		// final OtpErlangObject x = x2.elementAt(i);
		// r = r + pp(x) + ", ";
		// }
		// final String rr = r.length() > 2 ? r.substring(0, r.length() - 2) :
		// "";
		// return rr;
		final OtpErlangObject o = x2.elementAt(5);
		String result;
		if (o instanceof OtpErlangAtom) {
			final OtpErlangAtom a = (OtpErlangAtom) o;
			result = a.atomValue();
		} else {
			result = o.toString();
		}
		if (result.equals("undefined")) {
			final OtpErlangAtom a = (OtpErlangAtom) x2.elementAt(1);
			result = a.atomValue();
		}
		return result;
	}

	protected void logBuildStructure(final IResource resource) {
		if (ErlModelManager.verbose) {
			ErlLogger.debug("build structure for " + resource.getLocation());
		}
	}

	public static IErlElement getChildNamed(final IParent parent,
			final String name) {
		try {
			for (final IErlElement child : parent.getChildren()) {
				if (child.getName().equals(name)) {
					return child;
				}
			}
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
		return null;
	}

	public final void accept(final IErlElementVisitor visitor, final int flags,
			final IErlElement.Kind leafKind) throws ErlModelException {
		getModel().accept(this, visitor, flags, leafKind);
	}

}
