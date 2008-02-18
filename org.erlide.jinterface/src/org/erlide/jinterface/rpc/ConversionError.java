package org.erlide.jinterface.rpc;

public class ConversionError extends Exception {

	private static final long serialVersionUID = 1L;
	private final String fType;
	private final Object fObj;

	public ConversionError(Object obj, String type) {
		super();
		fObj = obj;
		fType = type;
	}

	@Override
	public String getMessage() {
		return "Conversion failed: " + fObj.toString() + " -> \"" + fType
				+ "\"";
	}

}
