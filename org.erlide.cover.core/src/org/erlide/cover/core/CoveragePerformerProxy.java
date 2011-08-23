package org.erlide.cover.core;

import org.erlide.cover.api.ICoverBackend;
import org.erlide.cover.api.ICoveragePerformer;
import org.erlide.cover.api.ICoveragePerformerProxy;

public class CoveragePerformerProxy implements ICoveragePerformerProxy {

    public ICoveragePerformer getPerformer() {
        return CoveragePerformer.getPerformer();
    }

    public ICoverBackend getBackend() {
        return CoverBackend.getInstance();
    }

}
