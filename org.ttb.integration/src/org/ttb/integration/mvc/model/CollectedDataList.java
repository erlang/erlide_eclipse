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

    private final List<ITreeNode> data = new ArrayList<ITreeNode>();
    private static final CollectedDataList INSTANCE = new CollectedDataList();

    private CollectedDataList() {
    }

    public static CollectedDataList getInstance() {
        return INSTANCE;
    }

    public List<ITreeNode> getData() {
        return data;
    }

    public void addData(ITreeNode _data) {
        data.add(_data);
    }

    public void clear() {
        data.clear();
    }

    public ITreeNode get(int index) {
        return data.get(index);
    }

    public int size() {
        return data.size();
    }
}
