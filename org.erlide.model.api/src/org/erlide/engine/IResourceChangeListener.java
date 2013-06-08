package org.erlide.engine;

public interface IResourceChangeListener {

    public abstract void handleChangedResources(ResourceChange delta);

}
