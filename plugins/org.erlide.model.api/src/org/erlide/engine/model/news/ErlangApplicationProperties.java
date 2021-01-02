package org.erlide.engine.model.news;

import java.util.Objects;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/**
 * From *.app
 */
@SuppressWarnings("all")
public class ErlangApplicationProperties {
    private final String version;

    public ErlangApplicationProperties(final String version) {
        this.version = version;
    }

    @Override
    public int hashCode() {
        return 31 * 1 + (version == null ? 0 : version.hashCode());
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ErlangApplicationProperties other = (ErlangApplicationProperties) obj;
        if (!Objects.equals(version, other.version)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("version", version);
        return b.toString();
    }

    public String getVersion() {
        return version;
    }
}
