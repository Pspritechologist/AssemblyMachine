local start_time = os.clock()

-- My lovely script :)

function hello(greeting)
	print(greeting)
end

function get_greeting()
	return "Hello, World!"
end

function awa()
	function hello()
		print("Hello, World!")
	end

	hello()
end

hello(get_greeting())

local greetings = {
	get_greeting(),
	"Hello, Universe!",
	"Hello, Multiverse!",
}

hello(greetings[1])
hello(greetings[2])
hello(greetings[3])

for i,greeting in ipairs(greetings) do
	hello(greeting)
end

local i = 12

while true do
	local ii = 0

	if ii % 10 == 0 then
		print("This number is cool~")
	else
		print("This number is lame.")
	end

	while true do
		ii = ii + 1
		if ii > i then break end
		if ii >= 50 then goto break_point end
	end

	i = i + 1
end
::break_point::

return os.clock() - start_time
