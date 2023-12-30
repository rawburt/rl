record Job(title: string, location: string)

record Person(name: string, age: number, job: Job)

def output_person(person: Person) do
  output(person.name)
  output(person.age)
  output(person.job.title)
  output(person.job.location)
end

def main do
  robert = Person(
    name: "Robert",
    age: 35,
    job: Job(
      title: "student",
      location: "Portland"
    )
  )
  # a birthday? sure!
  robert.age = 36
  # a new job? nice!
  robert.job.title = "mr manager"
  output_person(robert)
end
