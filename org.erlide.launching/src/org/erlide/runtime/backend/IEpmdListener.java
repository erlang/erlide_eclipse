package org.erlide.runtime.backend;

import java.util.List;

public interface IEpmdListener {

	void updateBackendStatus(String host, List<String> started,
			List<String> stopped);

}
