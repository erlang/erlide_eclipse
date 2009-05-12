package org.erlide.jinterface.util;

import java.util.List;

public interface IEpmdListener {

	void updateNodeStatus(String host, List<String> started,
			List<String> stopped);

}
