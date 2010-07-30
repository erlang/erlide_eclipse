package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Class representing traces collected from traced nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CollectedData {

    private final List<String> data = new ArrayList<String>();

    public List<String> getData() {
        return data;
    }

    public void addData(String _data) {
        data.add(_data);
    }
}
