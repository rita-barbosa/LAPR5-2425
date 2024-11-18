import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { catchError, map, Observable, of } from 'rxjs';
import { Specialization } from '../domain/specialization';
import { OperationType } from '../domain/OperationType';

@Injectable({
  providedIn: 'root'
})
export class OperationTypeService {

  theServerURL = 'https://localhost:5001/api';
  token = localStorage.getItem('user') ? JSON.parse(localStorage.getItem('user')!).token : null;
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
   };

  constructor(private messageService: MessageService, private http: HttpClient) { }

  public getAllSpecializationsAvailable(): Observable<string[]> {
    const url = `${this.theServerURL}/Specializations`;

    console.log(url)
  
    return this.http.get<Specialization[]>(url, this.httpOptions)
          .pipe(
            map(data => data.map(spec => spec.denomination)),
            catchError(this.handleError<string[]>('Get Specializations', []))
          );
  }

  createOperationType(operationType : OperationType) {
    const url = `${this.theServerURL}/OperationTypes`;

    console.log('Payload sent to backend:', JSON.stringify(operationType, null, 2));

    console.log(url)

    this.http.post<OperationType>(url, operationType, this.httpOptions)
      .pipe(catchError(this.handleError<OperationType>('Operation type creation')))
      .subscribe(data => {
        this.log(`Operation type: ${data.name} was successfully created.`);  
      });
  }

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if(error.status === 200 ||error.status === 400 ){
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured.`);
      }
      return of(result as T);
    };
  }
  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
