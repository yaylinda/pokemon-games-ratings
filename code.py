
import csv
import re
import requests


def get_html(url):
  """ Returns the HTML of the url page """

  r = requests.get(url, headers={
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 11_1_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36'
  })
  html = r.text

  return html


def parse_html(html):
  """ Parses the input HTML for Pokemon Game ratings"""

  lines = html.split('\n')

  data = []
  start_search = False
  start_parse_title = False

  for line in lines:

    if "main_stats" in line:
      start_search = True
      datum = {}

    elif '<span class="metascore_w medium game' in line and start_search:
      score_string = line.split('">')[1].split('</span>')[0]
      if score_string != 'tbd':
        datum['score'] = int(score_string)

    elif '<a href="/game/' in line and start_search:
      start_parse_title = True

    elif '' in line and start_search and start_parse_title:
      datum['title'] = line.strip()
      start_parse_title = False

    elif '<span class="platform">' in line and start_search:
      datum['platform'] = line.split('<span class="platform">')[1].split('</span>')[0]

    elif "Game, " in line and start_search:
      regex_results = re.findall("[0-9]{4}", line)

      if len(regex_results) > 0:
        datum['year'] = int(regex_results[0])

      start_search = False
      data.append(datum)

  return data


def write_csv(data):
  """ Writes a CSV file of the ratings data """

  with open('data.csv', 'w') as file:
    writer = csv.DictWriter(
        file, fieldnames=['title', 'score', 'platform', 'year'])
    writer.writeheader()
    for row in data:
      writer.writerow(row)


def main():
  base_url = 'https://www.metacritic.com/search/game/pokemon/results?page='
  num_pages = 22

  data = []

  for page_num in range(0, num_pages):
    url = base_url + str(page_num)
    html = get_html(url)
    print('Got HTML from %s. Parsing...' % url)

    page_data = parse_html(html)
    print(page_data)

    data.extend(page_data)

  print('Writing data to CSV...')
  write_csv(data)

  print('Done!')

if __name__ == '__main__':
    main()
