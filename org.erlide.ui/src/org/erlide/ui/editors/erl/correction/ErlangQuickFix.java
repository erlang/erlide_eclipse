package org.erlide.ui.editors.erl.correction;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;

public class ErlangQuickFix implements IMarkerResolution2 {

    private String label;
    private String description;
    private Image image;
    private List<String> tags;
    private IConfigurationElement element;
    private List<String> args;

    public ErlangQuickFix() {
    }

    public ErlangQuickFix(final ErlangQuickFix other) {
        label = other.getLabel();
        description = other.getDescription();
        image = other.getImage();
        tags = other.getTags();
        element = other.getConfigurationElement();
    }

    @Override
    public String getLabel() {
        return label;
    }

    public void setLabel(final String label) {
        this.label = label;
    }

    @Override
    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    @Override
    public Image getImage() {
        return image;
    }

    public void setImage(final Image image) {
        this.image = image;
    }

    public List<String> getTags() {
        return tags;
    }

    public void setTags(final List<String> tags) {
        this.tags = Lists.newArrayList(tags);
    }

    public IConfigurationElement getConfigurationElement() {
        return element;
    }

    public void setConfigurationElement(final IConfigurationElement element) {
        this.element = element;
    }

    public List<String> getArgs() {
        return args;
    }

    public void setArgs(final List<String> args) {
        this.args = args;
    }

    @Override
    public void run(final IMarker marker) {
        if (!marker.exists()) {
            return;
        }

        try {
            final Object object = element.createExecutableExtension("class");
            if (object instanceof ErlangQuickFixRunnable) {
                final ErlangQuickFixRunnable runnable = (ErlangQuickFixRunnable) object;
                runnable.setMarker(marker);
                runnable.setQuickFix(this);
                SafeRunner.run(runnable);
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

}
