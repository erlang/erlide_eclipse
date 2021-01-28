package org.erlide.ui.properties;

import java.util.Collection;

import org.eclipse.core.databinding.conversion.IConverter;
import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.root.PathSerializer;

public class StringToIPathListConverter implements IConverter<String, Collection<IPath>> {
    @Override
    public Object getToType() {
        return Collection.class;
    }

    @Override
    public Object getFromType() {
        return String.class;
    }

    @Override
    public Collection<IPath> convert(final String fromObject) {
        return PathSerializer.unpackList(fromObject);
    }
}
