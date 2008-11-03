package org.erlide.jinterface;

import org.osgi.framework.Bundle;

public interface ICodeBundle {

	Bundle getBundle();

	void start();

	String getEbinDir();

}
