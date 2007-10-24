package org.erlide.ui.navigator;

import org.eclipse.core.resources.IFile;
import org.erlide.core.erlang.IErlElement;

public class ErlangFileTreeData {

	private IFile container;
	private IErlElement elt;

	// private String name;
	// private String value;

	/**
	 * Create a property with the given name and value contained by the given
	 * file.
	 * 
	 * @param aName
	 *            The name of the property.
	 * @param aValue
	 *            The value of the property.
	 * @param aFile
	 *            The file that defines this property.
	 */
	public ErlangFileTreeData(IErlElement e, IFile aFile) {
		elt = e;
		container = aFile;
	}

	// public ErlangFileTreeData(String aName, String aValue, IFile aFile) {
	// name = aName;
	// value = aValue;
	// container = aFile;
	// }

	/**
	 * The name of this property.
	 * 
	 * @return The name of this property.
	 */
	// public String getName() {
	// return name;
	// }
	public String getName() {
		return elt.getElementName();
	}

	/**
	 * Return the value of the property in the file.
	 * 
	 * @return The value of the property in the file.
	 */
	public String getValue() {
		// return value;
		return elt.toString();
	}

	/**
	 * The IFile that defines this property.
	 * 
	 * @return The IFile that defines this property.
	 */
	public IFile getFile() {
		return container;
	}

	// public int hashCode() {
	// return name.hashCode();
	// }

	@Override
	public boolean equals(Object obj) {
		return obj instanceof ErlangFileTreeData
				&& ((ErlangFileTreeData) obj).getName().equals(getName());
	}

	@Override
	public String toString() {
		StringBuffer toString = new StringBuffer(getName())
				.append(":").append(getValue()); //$NON-NLS-1$
		return toString.toString();
	}

}
