type Day do
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
end

record Party(name: string, day: Day)

def show_day(day: Day) do
  output(day)
end

def main do
  party = Party(name: "Robert's Birthday Party", day: Friday)
  show_day(party.day)
end
