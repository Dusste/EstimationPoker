# Estimation Poker

### w/ Elm, Lamdera, TailwindCSS

#### Easily time estimate JIRA stories (or any tasks) by voting, just share the link with your team mates

### Flow:

- Enter your name (you will be room admin)
- Enter a room name
- Add stories that you want to estimate
- Land on choose story sequence step - there are common set of sequences or you can add your own custom sequence
- You are presented with certain range of numbers that represents story points for each story
- Share room link with your co-workers
- They will add their names and be redirected to voting page
- Voting will start once admin hit start timer
- Folks vote
- Admin actions: start timer, skip story, show votes (for others to see, they are hidden initially), reset timer, clear votes
- At the very end, when everyone have voted, 'finish voting'
- Donut and bar chart will be presented to represent percentage of votes
- Addional admin actions: on Voting step, you can edit stories names or room name on the fly, or add a new story

## Usage

```javascript
 npm i
 lamdera live
```

https://est-poker.lamdera.app
