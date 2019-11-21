# ruby-doc examples

puts "Forwardable\n------"
require 'forwardable'

class RecordCollection
    attr_accessor :records
    extend Forwardable
    # self.record_number(n) -> self.records[n]
    def_delegator :@records, :[], :record_number
end

r = RecordCollection.new
r.records = [1, 2, 3]
puts r.record_number(0) # 4

puts "Delegator\n------"
require 'delegate'

# forwards everything String has
del = SimpleDelegator.new "abc"
puts del
# Now number
del.__setobj__(5)
del.times do |x|
    puts x
end

class Message < DelegateClass(String)
    def message
        self.upcase
    end
end

us = Message.new("abcdef") # String initialised
puts us.message
us.each_char do |ch| # delegated to String
    puts ch
end