def fputs(str, f)
  File.open(f,'w') do |s|
    s.puts str
  end
end

def build_id
  ENV['BUILD_ID'] || ''
end

def generate_version_info
  version = `cat org.erlide/feature.xml | grep \"version=.*qualifier\" | head -n 1 | cut -d\\\" -f 2 | cut -d. -f1,2,3`
  info = `git describe`

  fputs "document.write('#{info.strip}');", "buildroot/info.js"
  fputs "document.write('#{version.strip}');", "buildroot/version.js"
  fputs "document.write('#{build_id.strip}');", "buildroot/id.js"
end

def run_ant(task, tools_dir="#{ENV['HOME']}/erlide_tools")
  system "export PATH=#{tools_dir}/bin:$PATH "+
  "&& export ANT_HOME=#{tools_dir}/ant "+
  "&& ant -f org.erlide.releng/build.ant #{task}"
end

def p2_repo(branch)
  case branch
    when "beta"
      "_beta"
    when "master"
      ""
  else
      "_nightly"
  end
end

def mirror_p2(from, to)
  puts "Mirroring: #{from} => #{to}"
  puts "TBD TBD TBD TBD TBD TBD TBD "
end