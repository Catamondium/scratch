require 'observer'

class MVal
    include Observable

    def initialize(val)
        @oldval = val
    end

    def val
        @oldval
    end

    def val=(newval)
        if @oldval != newval
            changed
            notify_observers(@oldval, newval)
            @oldval = newval
        end
    end
end

class Mon
    def update(oldval, newval)
        puts "Mon: #{oldval} -> #{newval}"
    end
end

x = MVal.new 55
x.add_observer(Mon.new)
x.val = 55 # no callback
x.val = "abc" # callback
x.val = 200