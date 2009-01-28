package org.erlide.jinterface;

import java.util.List;

public interface IEpmdListener {

	void updateNodeStatus(String host, List<String> started,
			List<String> stopped);

}
