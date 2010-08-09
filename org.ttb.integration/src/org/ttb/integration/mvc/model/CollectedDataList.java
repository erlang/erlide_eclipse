package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Class representing singleton set of traces collected from traced nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CollectedDataList {

    private final List<CollectedDataRoot> data = new ArrayList<CollectedDataRoot>();
    private static final CollectedDataList INSTANCE = new CollectedDataList();

    private CollectedDataList() {
    }

    public static CollectedDataList getInstance() {
        return INSTANCE;
    }

    public List<CollectedDataRoot> getData() {
        return data;
    }

    public void addData(CollectedDataRoot _data) {
        data.add(_data);
    }

    public void clear() {
        data.clear();
    }
}
