package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Class representing traces collected from traced nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CollectedDataList {

    private final List<CollectedDataRoot> data = new ArrayList<CollectedDataRoot>();

    public List<CollectedDataRoot> getData() {
        return data;
    }

    public void addData(CollectedDataRoot _data) {
        data.add(_data);
    }
}
