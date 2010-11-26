package org.erlide.jinterface.util;

import java.util.Collection;

public interface IEpmdListener {

    void updateNodeStatus(String host, Collection<String> started,
            Collection<String> stopped);

}
