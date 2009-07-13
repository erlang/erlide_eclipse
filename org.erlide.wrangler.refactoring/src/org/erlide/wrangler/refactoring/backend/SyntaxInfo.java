package org.erlide.wrangler.refactoring.backend;

public class SyntaxInfo {
	public SyntaxInfo(Type type, int definitionPosLine, int definitionPosColumn) {
		this.definitionPosColumn = definitionPosColumn;
		this.definitionPosLine = definitionPosLine;
		this.type = type;
	}

	protected int definitionPosLine;
	protected int definitionPosColumn;
	protected Type type;

	public boolean isVariable() {
		return type == Type.VARIABLE;
	}

	public enum Type {
		EXPRESSION, VARIABLE, NONE;
	}

}
