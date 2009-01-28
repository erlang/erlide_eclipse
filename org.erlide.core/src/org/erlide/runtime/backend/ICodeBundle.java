package org.erlide.runtime.backend;

import org.osgi.framework.Bundle;

public interface ICodeBundle {

	Bundle getBundle();

	void start();

	String getEbinDir();

}
