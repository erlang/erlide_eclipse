require 'find'
require 'pathname'
require 'fileutils'
require File.join(File.dirname(__FILE__), 'pde')

module Erlide
  def Erlide.build_id
    ENV['BUILD_ID'] || ''
  end

  def Erlide.generate_version_info(where)
    version = PDE.extractSourceFeatureVersion("org.erlide/feature.xml")
    info = `git describe`

    fputs "document.write('#{info.strip}');", "#{where}/info.js"
    fputs "document.write('#{PDE.unqualifiedVersion(version).strip}');", "#{where}/version.js"
    fputs "document.write('#{build_id.strip}');", "#{where}/id.js"
    fputs "#{PDE.unqualifiedVersion(version).strip}", "#{where}/version.txt"
  end

  def Erlide.run_ant(task, opts={}, tools_dir="#{ENV['HOME']}/erlide_tools")
    cmds = "export PATH=#{tools_dir}/bin:$PATH "+
    "&& export ANT_HOME=#{tools_dir}/ant "+
    "&& ant -f org.erlide.releng/build.ant #{definedOpts(opts)} #{task}"
    puts cmds
    system cmds
    if $?.exitstatus > 0 then
      raise "build failed"
    end
  end

  def Erlide.run_eclipse(target="#{ENV['HOME']}/erlide_tools/buckminster", opts={})
    launcher = File.glob("#{target}/plugins/org.eclipse.equinox.launcher_*.jar")[0]

    cmds = "export PATH=#{tools_dir}/bin:$PATH "+
    "&& export JAVA_HOME=#{tools_dir}/jdk "+
    "&& $JAVA_HOME/bin/java -jar #{launcher} #{spacedOpts(opts)} -verbose"
    puts cmds
    #system cmds
  end

  def Erlide.workspace_dir
    File.dirname(__FILE__)+"/../"
  end

  def Erlide.publish_site(source_dir, branch, output_base)
    puts "Publishing site..."

    version = PDE.getBuildVersion(source_dir)
    ts = PDE.getBuildTimestamp(source_dir)
    puts "  source_dir=#{source_dir}"

    system "umask 002"

    kind = p2_kind(branch)
    puts "  kind=#{kind}"
    if kind == "R"
      dest = "#{version}"
    else
      dest = "#{version}_#{kind}#{ts}"
    end

    puts "zip -r #{source_dir}/../erlide_#{dest}.zip #{source_dir}"
    Dir.chdir(source_dir) do
      system "zip -r #{source_dir}/../erlide_#{dest}.zip ."
    end
    puts "move #{source_dir}/../erlide_#{dest}.zip => #{source_dir}/erlide_#{dest}.zip"
    FileUtils.mv("#{source_dir}/../erlide_#{dest}.zip", "#{source_dir}/erlide_#{dest}.zip")

    repo = p2_repo_name(branch)
    full_dest = "#{output_base}/archive/#{repo}/#{dest}"
    puts "  output_dir=#{full_dest}"

    FileUtils.mkdir_p("#{full_dest}")
    FileUtils.cp_r("#{source_dir}/.", "#{full_dest}")
    FileUtils.chown_R(nil, "www-data", "#{full_dest}")

    if kind == "R"
      PDE.p2_add_composite("#{full_dest}", "#{output_base}")
      generate_version_info(output_base)
    else if kind != ""
        FileUtils.rm_f("#{output_base}/#{repo}")
        FileUtils.ln_s("#{full_dest}", "#{output_base}/#{repo}")
      end
    end

  end

  private

  def Erlide.fputs(str, f)
    File.open(f,'w') do |s|
      s.puts str
    end
  end

  def Erlide.definedOpts(map)
    map.reduce('') {|s, (k, v)| s << "-D#{k}=#{v} "}
  end

  def Erlide.spacedOpts(map)
    map.reduce('') {|s, (k, v)| s << "-#{k} #{v} "}
  end

  def Erlide.p2_repo_name(branch)
    case branch
    when "beta"
      "beta"
    when "release"
      "beta"
    when "master"
      "releases"
    else
      "nightly"
    end
  end

  def Erlide.p2_kind(branch)
    case branch
    when "pu"
      "A"
    when "beta"
      "B"
    when "release"
      "B"
    when "master"
      "R"
    else
      ""
    end
  end

end
