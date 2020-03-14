(function() {
  "use strict";

  // const tildePath = "m33.8 1c-25.6 0-32.8 23-32.8 34.6 0 11.6 15-6.8 31.6-6.8 16.6 0 31.5 20.3 53.6 20.3 25.6 0 32.8-23 32.8-34.6 0-11.6-15 6.8-31.6 6.8-16.6 0-31.5-20.3-53.6-20.3z"
  const tildePath = "m34.2 1c-25.3 0-36.4 23.7-32.4 34.6 3.9 10.7 16.5-6.3 31.1-6.8 16.4-0.6 31.1 20.3 52.9 20.3 25.3 0 36.4-23.7 32.4-34.6-3.9-10.7-16.5 6.3-31.1 6.8-16.4 0.6-31.1-20.3-52.9-20.3z"

  let [w, h, b] = [120, 50, 2]

  const styles = (style, color) => `stroke:${color};stroke-width:${b};fill:${color};fill-opacity:${style === "full" ? 1 : style === "half" ? 0.2 : 0}`
  const rectangle = (style, color) => `<rect x="${b/2}" y="${b/2}" width="${w - 2 * b}" height="${h - 2 * b}" style="${styles(style, color)}"/>`
  const tilde = (style, color) => `<path x="${b/2}" y="${b/2}" d="${tildePath}" style="${styles(style, color)}"/>`
  const ellipsis = (style, color) => `<ellipse cx="${w/2}" cy="${h/2}" rx="${(w-b)/2}" ry="${(h-b)/2}" style="${styles(style, color)}"/>`

  const SHAPES = [ellipsis, tilde, rectangle]
  const COLORS = ["#f44", "#2b2", "#66f"]
  const STYLES = ["full", "half", "empty"]
  const COUNTS = [1, 2, 3]

  function* zip(...arrays) {
    const len = Math.min(...arrays.map(arr => arr.length))
    for (let i = 0 ; i < len ; ++i) {
      yield arrays.map(arr => arr[i])
    }
  }

  function shuffle(array) {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      const temp = array[i];
      array[i] = array[j];
      array[j] = temp;
    }
    return array
  }

  function* combinations(head, ...tail) {
    if (tail.length === 0) {
      yield* head.map(item => [item])
    } else {
      for (const item of head) {
        for (const tailCombination of combinations(...tail)) {
          yield [item, ...tailCombination]
        }
      }
    }
  }

  const validate = cards => Array.from(zip(...cards)).every(props => new Set(props).size !== 2)

  function PlayerList(props) {
    const {players, selectedPlayer, lockedPlayers, onPlayerSelected} = props
    const playersElem = document.createElement("div")
    playersElem.id = "players"
    players.forEach(({name, score}, index) => {
      const div = document.createElement("div")
      div.classList.add("player")
      if (lockedPlayers.has(index)) {
        div.classList.add("locked")
      }
      if (index === selectedPlayer) {
        div.classList.add("selected")
      }
      if (onPlayerSelected && !lockedPlayers.has(index)) {
        div.addEventListener("click", _event => onPlayerSelected(index))
      }
      div.innerHTML = `<span class="player-name">${name}</span>`
                    + `<span class="player-score">${score || 0}</span>`
      playersElem.appendChild(div)
    })
    return playersElem
  }

  function CardsView(props) {
    const {cards, selectedCards, highlightedCards} = props
    const cardsElem = document.createElement("div")
    cardsElem.id = "cards"
    cards.forEach((card, index) => cardsElem.appendChild(Card({
        card,
        selected: selectedCards.has(index),
        highlighted: highlightedCards.has(index),
        onClick: _event => props.onCardSelected(index),
    })))
    return cardsElem
  }

  const card2svg = ([shape, count, style, color]) => (
       `<svg width="${w}" height="${h}" viewBox="0 0 ${w} ${h}" xmlns="http://www.w3.org/2000/svg">`
      + shape(style, color)
      + '</svg>'
    ).repeat(count)

  function Card(props) {
    const div = document.createElement("div")
    div.innerHTML = (card2svg(props.card))
    div.classList.add("card")
    if (props.selected) {
      div.classList.add("selected")
    }
    // if (props.highlighted) {
    //   div.classList.add("highlighted")
    // }
    if (props.onClick) {
      div.addEventListener("click", props.onClick)
    }
    return div
  }

  function Button(props) {
    const button = document.createElement("div")
    button.classList.add("button")
    button.innerHTML = props.text
    button.addEventListener("click", props.onClick)
    return button
  }


  function init() {
    const deck = shuffle(Array.from(combinations(SHAPES, COUNTS, STYLES, COLORS)))
    let state = {
      cards: deck.splice(0, 12),
      players: [{name: "Susanne", score: 0}, {name: "Philipp", score: 0}],
      selectedPlayer: undefined,
      lockedPlayers: new Set(),
      selectedCards: new Set(),
      moreCardsText: "Possible?",
      validTriple: new Set(),
    }

    const setState = async newState => {
      state = {...state, ...newState}
      console.log(state)
      render(state)
      update()
    }

    const onPlayerSelected = selectedPlayer => setState({selectedPlayer})

    const onCardSelected = card => {
      const selectedCards = new Set(state.selectedCards)
      selectedCards[selectedCards.has(card) ? "delete" : "add"](card)
      if (selectedCards.size <= 3) {
        setState({selectedCards})
      }
    }

    function update() {
      const { selectedPlayer, selectedCards} = state
      if (selectedCards.size === 3 && selectedPlayer !== undefined) {
        console.log("Valid Guess by player", selectedPlayer)
        if (validate(state.cards.filter((_, idx) => selectedCards.has(idx)))) {
          const cards = state.cards.slice()
          if (cards.length > 12) {
            cards = cards.filter((_, idx) => selectedCards.has(idx))
          } else {
            for (const idx of selectedCards) {
              cards[idx] = deck.pop()
            }
          }
          const players = state.players.slice()
          players[selectedPlayer].score += 3
          setState({
            cards,
            selectedCards: new Set(),
            players,
            selectedPlayer: undefined,
            lockedPlayers: new Set(),
            moreCardsText: "Possible?",
            validTriple: new Set(),
          })
        } else {
          console.log("Invalid Guess by player", selectedPlayer)
          let lockedPlayers = new Set(state.lockedPlayers)
          lockedPlayers[lockedPlayers.has(selectedPlayer) ? "delete" : "add"](selectedPlayer)
          let nextPlayer = undefined
          if (lockedPlayers.size === state.players.length) {
            lockedPlayers = new Set()
          } else if (lockedPlayers.size === state.players.length - 1) {
            nextPlayer = state.players.findIndex((_, idx) => !lockedPlayers.has(idx))
          }
          setState({
            lockedPlayers,
            selectedPlayer: nextPlayer,
            selectedCards: new Set(),
          })
        }
      }
    }

    function moreCards() {
      const {cards} = state
      for (let i = 0 ; i < cards.length - 2 ; ++i ) {
        for (let j = i + 1 ; j < cards.length - 1 ; ++j) {
          for (let k = j + 1 ; k < cards.length ; ++k) {
            const triple = [cards[i], cards[j], cards[k]]
            if (validate(triple)) {
              console.log("valid triple:", [i, j, k], triple)
              setState({
                moreCardsText: "Possible!",
                validTriple: new Set([i, j, k]),
              })
              return
            }
          }
        }
      }
      setState({cards: [...cards, ...deck.splice(0, 3)]})
    }


    const sidebar = document.getElementById("sidebar")
    let playerList = PlayerList({
      players: state.players,
      selectedPlayer: state.selectedPlayer,
      lockedPlayers: state.lockedPlayers,
      onPlayerSelected,
    })
    sidebar.appendChild(playerList)
    let moreCardsButton = Button({text: state.moreCardsText, onClick: moreCards})
    sidebar.appendChild(moreCardsButton)

    const main = document.getElementById("main")
    let cardsView = CardsView({
      cards: state.cards,
      selectedCards: state.selectedCards,
      highlightedCards: state.validTriple,
      onCardSelected,
    })
    main.appendChild(cardsView)

    function render(state) {
      const {players, selectedPlayer, lockedPlayers, cards, selectedCards, moreCardsText} = state
      const newPlayerList = PlayerList({players, selectedPlayer, lockedPlayers, onPlayerSelected})
      sidebar.replaceChild(newPlayerList, playerList)
      const newMoreCardsButton = Button({text: moreCardsText, onClick: moreCards})
      sidebar.replaceChild(newMoreCardsButton, moreCardsButton)
      moreCardsButton = newMoreCardsButton
      playerList = newPlayerList
      const newCardsView = CardsView({cards, selectedCards, highlightedCards: state.validTriple, onCardSelected})
      main.replaceChild(newCardsView, cardsView)
      cardsView = newCardsView
    }

  }

  addEventListener("load", init, {once: true})
})()
