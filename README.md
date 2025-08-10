# wyvern

<table>
  <tr>
    <td><img src="wyvern.png" alt="wyvern" style="width:300px;"/></td>
    <td>
      Simplified version of DRAKON diagramming language.
      <br /> <br />
      For a more faithful implementation of DRAKON, please visit: <a href="https://github.com/PiotrJustyna/drakon-renderer">PiotrJustyna/drakon-renderer</a>.
    </td>
  </tr>
</table>

## constraints

* only straight lines - no diagonal or curved lines, etc.
* main direction: top-to-bottom, loopbacks possible
* no arrows (direction is known and default) except loopbacks
* default path on the main skewer - the further to the right, the less ideal the scenario
* deterministic: input A will always produce output B

## development environment

The preferred way to work with wyvern is in containers. All scripts/commands described below will work directly on your hardware (not necessarily in a container), but the indended usage is in-container. 

| command | description |
| --- | --- |
| `./start-development-environment.sh` | starts a fully dockerized development environment |
| `./build.sh` | builds and lints code |
| `./run.sh` | runs code |
| `./format.sh` | formats all `*.hs` |
| `exit` | terminates development environment |

## community

* [youtube](https://www.youtube.com/playlist?list=PL9-WsOrOzOxSqWNqzhzyBGZsN0sOxEF6Q)
