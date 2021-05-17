module Data.Attribute exposing (Attribute(..), description, fromDegree, toString)


type Attribute
    = Observant
    | Active
    | Calculated
    | Intuitive
    | Selfless
    | Selfish
    | Energetic
    | Peaceful


fromDegree : Int -> Bool -> Attribute
fromDegree int bool =
    case int of
        1 ->
            if bool then
                Active

            else
                Observant

        2 ->
            if bool then
                Intuitive

            else
                Calculated

        3 ->
            if bool then
                Selfish

            else
                Selfless

        4 ->
            if bool then
                Peaceful

            else
                Energetic

        _ ->
            Observant


toString : Bool -> Attribute -> String
toString isGerman att =
    case att of
        Observant ->
            if isGerman then
                "Wachsam"

            else
                "Observant"

        Active ->
            if isGerman then
                "Aktiv"

            else
                "Active"

        Calculated ->
            if isGerman then
                "Kalkuliert"

            else
                "Calculated"

        Intuitive ->
            if isGerman then
                "Intuitiv"

            else
                "Intuitive"

        Selfless ->
            if isGerman then
                "Selbst-Fokusiert"

            else
                "Self-focused"

        Selfish ->
            if isGerman then
                "Selbstlos"

            else
                "Selfish"

        Energetic ->
            if isGerman then
                "Aufgeladen"

            else
                "Energetic"

        Peaceful ->
            if isGerman then
                "Friedlich"

            else
                "Peaceful"


description : Bool -> Attribute -> String
description isGerman att =
    case att of
        Observant ->
            if isGerman then
                "Du beobachtest wachsam die Leute um dich herum. "
                    ++ "Du wartest für den richtigen Moment um zur Tat zu schreiten."

            else
                "You are observant of the people around you. "
                    ++ "You are waiting for the right moment to act."

        Active ->
            if isGerman then
                "Du bist nimmst aktiv eine Rolle in dem Lebem um dich herum ein. "
                    ++ "Du hast Kontrolle über das Geschehen."

            else
                "You are actively taking a role in everything around you. "
                    ++ "You have control over things."

        Calculated ->
            if isGerman then
                "Du benützt deine Intelligenz zu deinem Vorteil. "
                    ++ "Du verfolgst einem Plan um zu erreichen was du willst."

            else
                "You use your intelligence to your advantage. "
                    ++ "You follow a plan how to get the things you want."

        Intuitive ->
            if isGerman then
                "Du gehst intuitiv durch dein Leben. "
                    ++ "Du folgst deinem Herz und deinen Träumen."

            else
                "You are intuitively going through your life. "
                    ++ "You follow your hearth and your dreams."

        Selfless ->
            if isGerman then
                "Du kümmerst dich selbstlos um andere. "
                    ++ "Du hast ein gutes Gefühl für die Bedürfnisse der Leute um dich herum."

            else
                "You are selflessly caring for others. "
                    ++ "You have good feeling for the needs of the people around you."

        Selfish ->
            if isGerman then
                "Du passt auf dich auf. "
                    ++ "Du gibst dir Zeit zu regenerieren."

            else
                "You are looking after yourself. "
                    ++ "You are given yourself time to regenerate."

        Energetic ->
            if isGerman then
                "Du bist voller Energie. "
                    ++ "Du kannst es mit jedem Hinderniss aufnehmen, dass dir in den Weg gelegt wird."

            else
                "You are full of energy. "
                    ++ "You are ready for any obstacle that comes in your way."

        Peaceful ->
            if isGerman then
                "Du lebst friedlich dein Leben. "
                    ++ "Du bist in Harmonie und eine Quelle für positive Energie."

            else
                "You are peaceful living your live. "
                    ++ "You are in harmony and a source for positive energy."
