package org.erlide.wrangler.refactoring.backend;

/**
 * Represents syntax information about a selection
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class SyntaxInfo {
	/**
	 * @param type
	 *            expression type
	 * @param definitionPosLine
	 *            expression starting position
	 * @param definitionPosColumn
	 *            expression starting column
	 */
	public SyntaxInfo(Type type, int definitionPosLine, int definitionPosColumn) {
		this.definitionPosColumn = definitionPosColumn;
		this.definitionPosLine = definitionPosLine;
		this.type = type;
	}

	protected int definitionPosLine;
	protected int definitionPosColumn;
	protected Type type;

	/**
	 * Returns true if the selection is variable
	 * 
	 * @return boolean
	 */
	public boolean isVariable() {
		return type == Type.VARIABLE;
	}

	/**
	 * An expression type
	 * 
	 * @author Gyorgy Orosz
	 * @version %I%, %G%
	 */
	public enum Type {
		EXPRESSION, VARIABLE, NONE;
	}

}
